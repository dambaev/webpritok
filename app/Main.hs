{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Web.Scotty as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Method as HTTP
  ( methodPost
  )
import qualified Network.HTTP.Types.Status as HTTP
  ( serviceUnavailable503
  , ok200
  )

import Control.Monad.IO.Class
  ( liftIO
  , MonadIO
  )

import qualified GHC.Stats as GS

import qualified Data.Aeson.Types as A
import qualified Network.Wai as Wai

import qualified Data.ByteString.Char8 as C8

import qualified Data.ByteString.Lazy.Char8 as LBS

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
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Zlib as C
import qualified Control.Monad.Trans.Resource as R
import qualified Network.Wai.Conduit as C
  ( sourceRequestBody
  )

import qualified Control.Monad.Trans.Either as E
  ( runEitherT
  , left
  , right
  , hoistEither
  )
import           Control.Monad.Trans.Either
  ( EitherT
  )

import Data.Binary.Builder
  ( Builder
  )

import qualified Data.Binary.Builder as B
  ( fromByteString
  )
import Control.Monad
  ( forM
  )
import Control.Monad.Trans.Class

import FirebirdDB

import System.IO

import Service


import Netstat

opts :: WS.Options
opts = WS.Options
  { verbose = 0
  , settings = Warp.setHost "0.0.0.0" Warp.defaultSettings
  }

main :: IO ()
main = do

  app <- WS.scottyApp myWebApp
  WS.scottyOpts opts $ WS.middleware $ myMiddleware app

myWebApp :: WS.ScottyM ()
myWebApp = do
    -- maybe, it should be authenticated too?
    WS.get "/status" $ doStatus
    
    -- first, authenticate get request
    WS.get (WS.function matchBadGetAuthenticate) doBadGetAuthenticate
    WS.get "/service/state/:name" $ doServiceStatus
    WS.get "/service/stop/:name" doServiceStop
    WS.get "/service/start/:name" doServiceStart
    WS.get "/service/kill/:name" doServiceKill
    WS.get "/app/start" doAppStart
    WS.get "/app/stop" doAppStop
    WS.get "/app/status" doAppStatus
    WS.get (WS.function $ \_-> Just []) $ handleGet

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

handleGet:: WS.ActionM ()
handleGet = do
  WS.html "handleGet"

-- | will return Nothing if hash is presents and matches to request
matchBadGetAuthenticate:: Wai.Request-> Maybe [WS.Param]
matchBadGetAuthenticate req = Nothing {- let
--  reqNoHash = getUrlWithoutHash
  in case mhash of
       Nothing-> Just []
       Just hash ->
         if hash == rehashed
           then Nothing
           else Just []
     where
       query = Wai.queryString req
       mhash = case filter (\(key, _) -> key == "hash") query of
         [] -> Nothing
         ((_, hash):_) -> case hash of
                            Nothing -> Nothing
                            Just some -> Just some
-}
                                
doBadGetAuthenticate:: WS.ActionM ()
doBadGetAuthenticate = WS.status HTTP.serviceUnavailable503

  

-- | checks 
doServiceStatus:: WS.ActionM ()
doServiceStatus = do
  name <- WS.param "name"
  estate <- liftIO $ E.runEitherT $ withSCM scmAccessRead $ \scm-> getServiceStateW scm name
  case estate of
    Right status -> WS.json status
    Left err -> do
      WS.status HTTP.serviceUnavailable503
      WS.text err
  where
    
      

-- | checks 
doServiceStart:: WS.ActionM ()
doServiceStart = do
  name <- WS.param "name"
  merr <- liftIO $ E.runEitherT $ withSCM scmAccessAll $ \scm-> startServiceSyncW scm name
  case merr of
    Right _ -> do
      WS.status HTTP.ok200
      WS.text ""
    Left err -> do
      WS.status HTTP.serviceUnavailable503
      WS.text err

-- | checks 
doServiceStop:: WS.ActionM ()
doServiceStop = do
  name <- WS.param "name"
  merr <- liftIO $ E.runEitherT $ withSCM scmAccessAll $ \scm-> stopServiceSyncW scm name
  case merr of
    Right _ -> WS.status HTTP.ok200
    Left err -> do
      WS.status HTTP.serviceUnavailable503
      WS.text err

uploadDB :: Wai.Application 
uploadDB request respond = do
  -- check for free space, header and etc
  eret <- E.runEitherT $ do
    checkFreeSpace
    uploadTempFile request
  
  case eret of
    Left err  -> respond $ Wai.responseLBS HTTP.serviceUnavailable503 [] $ 
      LBS.pack err
    Right _ -> respond $ Wai.responseLBS HTTP.ok200 [] ""

checkFreeSpace:: EitherT String IO ()
checkFreeSpace = E.right ()  

uploadTempFile 
  :: Wai.Request 
  -> EitherT String IO ()
uploadTempFile request = do
  (src, check) <- lift 
    $ R.runResourceT 
    $ C.sourceRequestBody request 
    $$+ checkHeaderC -- check if header is fit
  if not check
    then E.left "check failed"
    else do
        lift $ R.runResourceT $ src $$+- C.sinkFile "test2"
        E.right ()
  


myMiddleware :: Wai.Application -> Wai.Application-> Wai.Application
myMiddleware app app1 request respond = do
  case () of
    _ | Wai.requestMethod request == HTTP.methodPost -> do
        if Wai.pathInfo request == [ "app", "pritok", "uploaddb" ]
          then uploadDB request respond
          else app request respond
    _ -> app request respond

doServiceKill :: WS.ActionM ()
doServiceKill = do
  name <- WS.param "name"
  merr <- liftIO $ E.runEitherT $ withSCM scmAccessAll $ killServiceW name
  case merr of
    Right _ -> WS.status HTTP.ok200
    Left err -> do
      WS.status HTTP.serviceUnavailable503
      WS.text err

doDownloadDB :: WS.ActionM ()
doDownloadDB = do
  efile <- E.runEitherT backupDB
  case efile of
    Left _ -> WS.status HTTP.serviceUnavailable503
    Right file -> WS.stream $ \write flush ->
      R.runResourceT $ C.sourceFile file $$ C.gzip =$ streamConsumer write flush
  where
    streamConsumer :: MonadIO m 
      => (Builder -> IO ())
      -> IO ()
      -> Sink C8.ByteString m ()
    streamConsumer write flush = awaitForever $ \buff -> liftIO $ do
      write $ B.fromByteString buff
      flush

backupDB :: MonadIO m => EitherT String m FilePath
backupDB = 
    -- execute gbak and wait for timeout
    E.left "error"

doAppStart :: WS.ActionM ()
doAppStart = do
    eret <- liftIO $ E.runEitherT $ withSCM scmAccessAll $ \scm-> do
      startServiceSyncW scm "FirebirdServerDefaultInstance"
      startServiceSyncW scm "prt_ManagerDB"
      startServiceSyncW scm "prt_Kernel"
      startServiceSyncW scm "prt_ApMonitor"
    case eret of
      Left err -> do
        WS.status HTTP.serviceUnavailable503
        WS.text err
      Right _ -> do
        WS.status HTTP.ok200
        WS.text ""

doAppStop :: WS.ActionM ()
doAppStop = do
  eret <- liftIO $ E.runEitherT $ withSCM scmAccessAll $ \scm-> do
    let stopOrKillSvc name delay = withService scm name serviceAccessAll $ \svc-> after delay
          (stopServiceSync svc) (killService svc)
    stopOrKillSvc "prt_ApMonitor" 5000000
    stopOrKillSvc "prt_Kernel" 5000000 
    stopOrKillSvc "prt_ManagerDB" 5000000
    stopOrKillSvc "FirebirdServerDefaultInstance" 5000000
  case eret of
    Left err -> do
      WS.status HTTP.serviceUnavailable503
      WS.text err
    Right _ -> do
      WS.status HTTP.ok200
      WS.text ""

doAppStatus :: WS.ActionM ()
doAppStatus = do
  estates <- liftIO $ E.runEitherT worker
  case estates of
    Left err -> do
      WS.status HTTP.serviceUnavailable503
      WS.text err
    Right states -> do
      let status = if all (== SS_Running) states
            then HTTP.ok200
            else HTTP.serviceUnavailable503
      WS.status status
  where
    services = [ "FirebirdServerDefaultInstance", "prt_ManagerDB", "prt_Kernel", "prt_ApMonitor"]
    worker :: EitherT Error IO [ServiceState]
    worker = withSCM scmAccessRead $ \scm-> forM services $ getServiceStateW scm

