
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Service
  where


import qualified Data.ByteString.Char8 as C8
import qualified Language.C.Inline as C
import qualified Control.Concurrent as C
import qualified System.Timeout as T
import qualified Control.Monad.Catch as E

import Data.Word
  ( Word32
  )

import Foreign.Ptr
  ( Ptr
  , nullPtr
  )

import Data.Monoid ((<>))
import Data.Either
import Data.Text.Lazy as T
import Data.List as L

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Types



killServiceW
  :: ( Monad m
     , ServiceOpener hscm hsvc m
     , ServiceController hscm hsvc m
     , E.MonadMask m
     )
  => C8.ByteString
  -> hscm
  -> m ()
killServiceW name scm = withService scm name serviceAccessQuery killService


withSCM
  :: ( Monad m
     , ServiceOpener hscm hsvc m
     , SCMOpener hscm m
     , E.MonadMask m
     )
  => SCMAccess
  -> ( hscm
     -> m a
     )
  -> m a
withSCM access inner = E.bracket (openSCM access) closeSCM inner

withService
  :: ( Monad m
     , ServiceOpener hscm hsvc m
     , E.MonadMask m
     )
  => hscm
  -> C8.ByteString
  -> ServiceAccess
  -> ( hsvc
     -> m a
     )
  -> m a
withService hscm name access inner = E.bracket 
    (openService hscm name access) 
    closeService 
    inner

waitForSvcStatus
  :: ( Monad m
     , ServiceReader hsvc m
     , ThreadDelay m
     )
  => hsvc
  -> ServiceState
  -> m ()
waitForSvcStatus svc desiredState = do
  state <- getServiceState svc
  if state /= desiredState
    then do
      threadDelay 200000
      waitForSvcStatus svc desiredState
    else return ()

startServiceSyncW
  :: ( ServiceController hscm hsvc m
     , E.MonadMask m
     , ServiceOpener hscm hsvc m
     , ThreadDelay m
     )
  => hscm
  -> C8.ByteString
  -> m ()
startServiceSyncW scm name = withService scm name serviceAccessStart $ \svc -> do
  startService svc
  waitForSvcStatus svc SS_Running

stopServiceSync
  :: (ServiceController hscm hsvc m
     , ThreadDelay m
     )
  => hsvc
  -> m ()
stopServiceSync svc = do
  stopService svc
  waitForSvcStatus svc SS_Stopped

stopServiceSyncW
  :: ( ServiceController hscm hsvc m
     , E.MonadMask m
     , ServiceOpener hscm hsvc m
     , ThreadDelay m
     )
  => hscm
  -> C8.ByteString
  -> m ()
stopServiceSyncW scm name = withService scm name serviceAccessStop stopServiceSync

getServiceStateW
  :: ( ServiceReader hsvc m 
     , ServiceOpener hscm hsvc m
     , E.MonadMask m
     )
  => hscm
  -> C8.ByteString
  -> m ServiceState
getServiceStateW scm name = withService scm name serviceAccessQuery getServiceState


after
  :: ( Monad m
     , Timeout m
     )
  => Int
  -> m a
  -> m a
  -> m a
after delay payload timeoutWorker = do
  mret <- timeout delay payload
  case mret of
    Nothing -> timeoutWorker
    Just some ->  return some


