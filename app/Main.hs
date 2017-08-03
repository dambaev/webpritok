{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Web.Scotty as WS
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

import Service

main :: IO ()
main = WS.scotty 3000 $ do
  -- maybe, it should be authenticated too?
  WS.get "/status" $ doStatus
  
  -- first, authenticate get request
  WS.get (WS.function matchBadGetAuthenticate) doBadGetAuthenticate
  WS.get "/service/running/:name" $ doServiceStatus
  WS.get (WS.function $ \_-> Just []) $ handleGet

  -- post request



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
  liftIO $ print "service status"
  name <- WS.param "name"
  running <- isServiceRunning name
  if not running
    then WS.status $ HTTP.serviceUnavailable503
    else WS.status $ HTTP.ok200
