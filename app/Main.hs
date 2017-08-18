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
  )

import qualified GHC.Stats as GS

import qualified Data.Aeson.Types as A
import qualified Network.Wai as Wai

import qualified Data.ByteString.Char8 as C8

import qualified Data.ByteString.Lazy as LBS

import           Data.Conduit
  ( ($$)
  )
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Control.Monad.Trans.Resource as R
import qualified Network.Wai.Conduit as C
  ( sourceRequestBody
  )

import System.IO

import Service

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
  --  WS.get "/app/pritok/status"
    WS.get (WS.function $ \_-> Just []) $ handleGet

  -- post request
--  WS.post (WS.function $ \_-> Nothing) undefined
--    WS.post "/app/pritok/uploaddb" uploadDB


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
doBadGetAuthenticate = WS.status $ HTTP.serviceUnavailable503

  

-- | checks 
doServiceStatus:: WS.ActionM ()
doServiceStatus = do
  name <- WS.param "name"
  estate <- liftIO $ getServiceState name
  case estate of
    Right status -> WS.json status
    Left err -> do
      WS.status $ HTTP.serviceUnavailable503
      WS.text err
      

-- | checks 
doServiceStart:: WS.ActionM ()
doServiceStart = do
  name <- WS.param "name"
  merr <- liftIO $ startService name
  case merr of
    Nothing -> WS.status $ HTTP.ok200
    Just err -> do
      WS.status $ HTTP.serviceUnavailable503
      WS.text err

-- | checks 
doServiceStop:: WS.ActionM ()
doServiceStop = do
  name <- WS.param "name"
  merr <- liftIO $ stopService name
  case merr of
    Nothing -> WS.status $ HTTP.ok200
    Just err -> do
      WS.status $ HTTP.serviceUnavailable503
      WS.text err

uploadDB :: Wai.Application 
uploadDB request respond = do
  -- check for free space, header and etc
  R.runResourceT $ C.sourceRequestBody request $$ C.sinkFile "test2"
  respond $ Wai.responseLBS HTTP.ok200 [] ""
  

myMiddleware :: Wai.Application -> Wai.Application-> Wai.Application
myMiddleware app app1 request respond = do
  case () of
    _ | Wai.requestMethod request == HTTP.methodPost -> do
        if Wai.pathInfo request == [ "app", "pritok", "uploaddb" ]
          then uploadDB request respond
          else app request respond
    _ -> app request respond
