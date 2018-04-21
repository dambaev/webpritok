{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Web.Scotty as WS
import qualified Web.Scotty.Internal.Types as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Method as HTTP
  ( methodPost
  )
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Zlib as C
import Data.Binary.Builder
import qualified Network.Wai.Conduit as C
  ( sourceRequestBody
  )
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Builder as B
  ( fromByteString
  )
import Control.Monad.IO.Class
import           Data.Conduit
  ( ($$)
  , ($$+)
  , ($$+-)
  , (=$$+)
  , (=$$+-)
  , (=$)
  , Conduit
  , Sink
  , awaitForever
  )

import qualified Network.HTTP.Types.Status as HTTP
  ( serviceUnavailable503
  , ok200
  )

import Control.Monad.IO.Class
  ( liftIO
  , MonadIO
  )
import Control.Monad.Catch as E

import qualified GHC.Stats as GS

import qualified Data.Aeson.Types as A
import qualified Network.Wai as Wai

import Data.ByteString.Char8
  ( ByteString
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


import Data.Binary.Builder
  ( Builder
  )

import Control.Monad
  ( forM
  , when
  )
import Control.Monad.Trans.Class
import qualified Control.Exception as CE
import VTVar
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as C
import System.Random (StdGen)
import qualified System.Random as R

import qualified Data.Foldable as F
import qualified Data.Digest.Pure.SHA as S
import qualified Debug.Trace as D
import Control.Exception
  ( SomeException
  )

import Types

import FirebirdDB

import System.IO

import Service
import ServiceIO 
    ( EServiceStopFailed(..)
    )
import System.FilePath
import Netstat
import Interface
import Production

type SessionId = Text
type Sessions = Map SessionId (VTVar UTCTime)
data State = State
  { sSessions:: VTVar Sessions
  , sStdGen:: VTVar StdGen
  }

timeout:: Int
timeout = 5

opts :: WS.Options
opts = WS.Options
  { verbose = 0
  , settings = Warp.setHost "0.0.0.0" Warp.defaultSettings
  }

main :: IO ()
main = do
  state <- newState
  salt <- getSalt
  app <- WS.scottyApp (myWebApp state salt)
  WS.scottyOpts opts $ WS.middleware $ myMiddleware app
  where
    newState = do
      sessions <- liftIO $ runWSTM $ atomicallyW $ newVTVar M.empty
      stdgen <- R.getStdGen
      stdgenT <- liftIO $ runWSTM $ atomicallyW $ newVTVar stdgen
      return $ State 
        { sSessions = sessions
        , sStdGen = stdgenT
        }
    getSalt = do
      eread <- E.try $ readFile "etc/salt"
      return $ case eread of
        Left (e::SomeException) -> "0987653212"
        Right (some::String)-> C8.pack $ head $ lines some


myWebApp 
    :: State
    -> ByteString
    -> WS.ScottyM ()
    
myWebApp state salt = do
    -- maybe, it should be authenticated too?
    -- first, authenticate get request
    WS.get "/listsids" $ doListSessions state
    WS.get (WS.function $ matchBadGetAuthenticate salt) $ doBadGetAuthenticate salt
    WS.get "/status" $ doStatus
    
    WS.get "/getsid" $ doGetSid state
    
    

    WS.get "/service/state/:name" $ doServiceStatus state
    WS.get "/service/stop/:name" $ doServiceStop state
    WS.get "/service/start/:name" $ doServiceStart state
    WS.get "/service/kill/:name" $ doServiceKill state
    WS.get "/app/start" $ doAppStart state
    WS.get "/app/stop" $ doAppStop state
    WS.get "/app/status" $ doAppStatus state
    WS.get "/app/backupdb/:dbpath" $ doBackupDB state
    WS.get "/app/getdb" $ doAppGetDB state
    WS.get (WS.function $ \_-> Just []) $ handleGet state

  -- post request
    -- WS.post (WS.function $ \_-> Nothing) undefined
    -- WS.post "/app/pritok/uploaddb" uploadDB


-- GS.GCStats should be representable as JSON data  
instance A.ToJSON GS.GCStats where
  toJSON (GS.GCStats {..}) = A.object
    [ "bytesAllocated" A..= bytesAllocated
    , "numGcs" A..= numGcs
    , "maxBytesUsed" A..= maxBytesUsed
    , "numByteUsageSamples" A..= numByteUsageSamples
    , "cumulativeBytesUsed" A..= cumulativeBytesUsed
    , "bytesCopied" A..= bytesCopied
    , "currentBytesUsed" A..= currentBytesUsed
    , "currentBytesSlop" A..= currentBytesSlop
    , "maxBytesSlop" A..= maxBytesSlop
    , "peakMegabytesAllocated" A..= peakMegabytesAllocated
    , "mutatorCpuSeconds" A..= mutatorCpuSeconds
    , "mutatorWallSeconds" A..= mutatorWallSeconds
    , "gcCpuSeconds" A..= gcCpuSeconds
    , "gcWallSeconds" A..= gcWallSeconds
    , "cpuSeconds" A..= cpuSeconds
    , "wallSeconds" A..= wallSeconds
    , "parTotBytesCopied" A..= parTotBytesCopied
    , "parMaxBytesCopied" A..= parMaxBytesCopied
    ]

doStatus:: WS.ActionM ()
doStatus = do
  enabled <- liftIO $ GS.getGCStatsEnabled
  if not enabled
    then WS.status $ HTTP.serviceUnavailable503
    else do
      stats <- liftIO $ GS.getGCStats
      WS.json $ stats

handleGet:: State-> WS.ActionM ()
handleGet state = do
  expireSessions state
  WS.html "handleGet"

-- | will return Nothing if hash is presents and matches to request
matchBadGetAuthenticate:: ByteString-> Wai.Request-> Maybe [WS.Param]
matchBadGetAuthenticate salt req = case mqueryHash of
    Nothing        -> Just []
    Just queryHash | queryHash == calculatedHash -> D.trace 
        ("queryHash = " ++ BS.unpack queryHash ++ ", calculated ++ " ++ BS.unpack calculatedHash) Nothing
    _ -> Just []
  where
       query
          = if C8.length queryNoHashStr > 0
          then Wai.rawPathInfo req 
            `C8.append` "?"
            `C8.append` queryNoHashStr
          else Wai.rawPathInfo req
       queryNoHash = reverse queryNoHash'
       (mqueryHash, queryNoHash')
          = F.foldl' helper (Nothing,[]) $ Wai.queryString req
       helper (hashTmp, tmp) item@(key, mval) = case key of
          "hash" -> (mval, tmp)
          _ -> let
            itemstr = case mval of
              Nothing-> key
              Just val -> key `C8.append` "=" `C8.append` val
            in (hashTmp, itemstr:tmp)
       queryNoHashStr = C8.intercalate "&" queryNoHash
       calculatedHash
          = C8.pack 
          $ S.showDigest
          $! S.sha256
          $ LBS.fromStrict
          $ query `C8.append` salt

doBadGetAuthenticate:: ByteString-> WS.ActionM ()
doBadGetAuthenticate salt = do
  req <- WS.request
  let requestNoSalt
          = if C8.length queryNoHashStr > 0
          then Wai.rawPathInfo req 
            `C8.append` "?"
            `C8.append` queryNoHashStr
          else Wai.rawPathInfo req
      queryNoHash = reverse queryNoHash'
      (mqueryHash, queryNoHash')
          = F.foldl' helper (Nothing,[]) $ Wai.queryString req
      helper (hashTmp, tmp) item@(key, mval) = case key of
          "hash" -> (mval, tmp)
          _ -> let
            itemstr = case mval of
              Nothing-> key
              Just val -> key `C8.append` "=" `C8.append` val
            in (hashTmp, itemstr:tmp)
      queryNoHashStr = C8.intercalate "&" queryNoHash
      calculatedHash
          = C8.pack 
          $ S.showDigest
          $! S.sha256
          $ LBS.fromStrict
          $ requestSalt
      requestSalt = requestNoSalt `C8.append` salt
  WS.text "bad hash"
  WS.status HTTP.serviceUnavailable503
  liftIO $ C8.putStrLn "bad hash"
  liftIO $ C8.putStrLn requestSalt
  liftIO $ C8.putStrLn $ Wai.rawQueryString req
  liftIO $ C8.putStrLn calculatedHash

  

-- | checks 
doServiceStatus:: State-> WS.ActionM ()
doServiceStatus state = do
  expireSessions state
  name <- WS.param "name"
  state <- liftIO $ withSCM scmAccessRead $ \scm-> getServiceStateW scm name
  WS.json state
      

-- | checks 
doServiceStart:: State-> WS.ActionM ()
doServiceStart state = do
  expireSessions state
  name <- WS.param "name"
  liftIO $ withSCM scmAccessAll $ \scm-> startServiceSyncW scm name
  WS.status HTTP.ok200

-- | checks 
doServiceStop:: State-> WS.ActionM ()
doServiceStop state = do
  expireSessions state
  name <- WS.param "name"
  liftIO $ withSCM scmAccessAll $ \scm-> stopServiceSyncW scm name
  WS.status HTTP.ok200

uploadDB :: Wai.Application 
uploadDB request respond = do
  -- check for free space, header and etc
  -- checkFreeSpace
  uploadTempFile request
  
  respond $ Wai.responseLBS HTTP.ok200 [] ""

data DBWrongHeader = DBWrongHeader
  deriving (Show)
instance Exception DBWrongHeader

uploadTempFile 
  :: Wai.Request 
  -> IO ()
uploadTempFile request = do
  (src, check) <- R.runResourceT 
    $ C.sourceRequestBody request 
    $$+ checkHeaderC -- check if header is fit
  when (not check) $ throwM DBWrongHeader
  R.runResourceT $ src $$+- C.sinkFile "test2"
  return ()
  


myMiddleware :: Wai.Application -> Wai.Application-> Wai.Application
myMiddleware app app1 request respond = do
  case () of
    _ | Wai.requestMethod request == HTTP.methodPost -> do
        if Wai.pathInfo request == [ "app", "pritok", "uploaddb" ]
          then uploadDB request respond
          else app request respond
    _ -> app request respond

doServiceKill :: State-> WS.ActionM ()
doServiceKill state = do
  expireSessions state
  name <- WS.param "name"
  liftIO $ withSCM scmAccessAll $ killServiceW name
  WS.status HTTP.ok200

doBackupDB :: State-> WS.ActionM ()
doBackupDB state = do
  expireSessions state
  dbpath <- WS.param "dbpath"
  -- liftIO $ runBackup dbpath
  WS.stream $ runBackup dbpath


runBackup
  :: ( MonadIO m
     , UsesHTTPFileResponse m
     , ForksChild m
     , ReadsFile m
     , DeletesFile m
     )
  => ByteString
  -> (Builder-> IO ())
  -> IO ()
  -> m ()
runBackup dbpath write flush = do
  doesFileExist file >>= \flag-> when flag $ removeFile file
  runGBAK
  downloadFile file write flush
  where
    file = C8.pack $ replaceFileName (C8.unpack dbpath) "webpritok.tmp"
    runGBAK = do
      runProgram "gbak" []
  


doAppStart :: State-> WS.ActionM ()
doAppStart state = do
  expireSessions state
  liftIO $ withSCM scmAccessAll $ \scm-> mapM_ (helper scm) services
  WS.status HTTP.ok200
  where
    helper scm svc = do
        putStrLn $ "starting " ++ (BS.unpack svc)
        startServiceSyncW scm svc
    services = [ "FirebirdServerDefaultInstance"
               , "prt_ManagerDB"
               , "prt_Kernel"
               , "prt_ApMonitor"
               ]

doAppStop :: State-> WS.ActionM ()
doAppStop state = do
  expireSessions state
  liftIO $ withSCM scmAccessAll $ \scm-> do
    mapM_ (stopOrKillSvc scm 5000000) services
  WS.status HTTP.ok200
  where
    stopOrKillSvc scm delay name = withService scm name serviceAccessAll $ \svc-> do
        putStrLn $ "stopping " ++ (BS.unpack name)
        E.handle (\EServiceStopFailed-> killService svc) 
          $ after delay (stopServiceSync svc) (killService svc)
    services = [ "prt_ApMonitor", "prt_Kernel", "prt_ManagerDB"
               , "FirebirdServerDefaultInstance"]

doAppStatus :: State-> WS.ActionM ()
doAppStatus state = do
  expireSessions state
  req <- WS.request
  let reqPath = Wai.rawPathInfo req `C8.append` Wai.rawQueryString req
  ret <- liftIO $ do
    estates <- worker
    listen <- isTcpPortListening 6000
    return $ case (listen, estates) of
      (False, _)      -> False
      ( True, states) 
        | all (== SS_Running) states -> True
        | otherwise -> False
  WS.status $ if ret 
    then HTTP.ok200
    else HTTP.serviceUnavailable503
  where
    services = [ "FirebirdServerDefaultInstance", "prt_ManagerDB", "prt_Kernel", "prt_ApMonitor"]
    worker :: IO [ServiceState]
    worker = withSCM scmAccessRead $ \scm-> forM services $ getServiceStateW scm

doGetSid 
  :: State
  -> WS.ActionM ()
doGetSid state = do
  expireSessions state
  Just sessionId <- snd <$> (liftIO $ rcuVTVar readvars calc write)
  WS.status HTTP.ok200
  WS.text sessionId
  return ()
  where
    readvars = do
      currtime <- RSTM $ C.getCurrentTime
      atomicallyR $ do
        first<- readVTVar $ sStdGen state 
        second <- readVTVar $ sSessions state 
        return (currtime,first,second)
    calc (currtime, (stdgen0, stdgenV), sessions@(sessions0, sessionsV)) = 
        let (digitsStr, newstdgen) = genDigits stdgen0 6 []
        in case M.lookup digitsStr sessions0 of
          Nothing-> ((), Just (currtime, digitsStr, (newstdgen, stdgenV), sessions))
          Just _ -> calc (currtime, (newstdgen, stdgenV), sessions)
    write (currtime,sessionId, stdgenI, (sessions0,sessionsV)) = do
      currtimeT <- newVTVar currtime
      writeVTVar (sStdGen state) stdgenI
      writeVTVar (sSessions state) (M.insert sessionId currtimeT sessions0, sessionsV)
      return sessionId

    genDigits
      :: StdGen
      -> Int
      -> [Int]
      -> (Text, StdGen)
    genDigits stdgen 0 tmp = (T.pack $ concatMap show tmp, stdgen)
    genDigits stdgen n tmp = genDigits newstdgen newn newtmp
      where 
        (newdigit, newstdgen) = R.randomR (0,9) stdgen
        newn = n - 1
        newtmp = newdigit:tmp

expireSessions
  :: ( MonadIO m
     )
  => State
  -> m ()
expireSessions state = do
  liftIO $ ruVTVar readvars write
  return ()
  where
    sessionsT = sSessions state
    readvars = do
      currtime <- RSTM $ C.getCurrentTime
      (sessions0,sessionsV) <- atomicallyR $ readVTVar sessionsT
      let helper tmp (sessionId,timeT)  = do
            (sessTime, _ ) <- atomicallyR $ readVTVar timeT
            let secs = truncate $ C.diffUTCTime currtime sessTime
            return $ if secs < Main.timeout
              then tmp
              else sessionId:tmp
      sessions <- F.foldlM helper [] $ M.toList sessions0
      let newSessions = F.foldl' (\m k-> M.delete k m) sessions0 sessions
      return ( (), Just ( newSessions, sessionsV))
    write = writeVTVar sessionsT
    

doListSessions:: State-> WS.ActionM ()
doListSessions state = do
  expireSessions state
  (sessions0, _) <- liftIO $ runRSTM $ atomicallyR $ readVTVar $ sSessions state
  sessions <- forM (M.toList sessions0) $ \(session, timeT)-> do
    (time0, _) <- liftIO $ runRSTM $ atomicallyR $ readVTVar timeT
    return $ session `T.append` ": " `T.append` ( T.pack $ show time0)
  WS.status HTTP.ok200
  WS.text $ T.concat sessions

doAppGetDB:: State-> WS.ActionM ()
doAppGetDB state = do
  expireSessions state
  

