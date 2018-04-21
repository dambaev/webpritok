{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Types 
    where

import Data.ByteString.Char8
import qualified Language.C.Inline as C
import Data.Monoid ((<>))

import qualified Data.Aeson.Types as A
  ( toJSON
  , ToJSON
  , object
  , (.=)
  )

import Data.Text.Lazy as T

C.context (C.baseCtx <> C.bsCtx)

C.include "windows.h"
C.include "winsvc.h"


type Error = Text



data ServiceState
  = SS_Stopped
  | SS_StopPending
  | SS_StartPending
  | SS_Running
  | SS_Paused
  | SS_PausePending
  | SS_ContinuePending
  deriving Eq

instance A.ToJSON ServiceState where
  toJSON state = A.object
    [ "service_state" A..= state_str
    ]
    where
      state_str:: Text
      state_str = case state of
        SS_Stopped -> "stopped"
        SS_StopPending -> "stop_pending"
        SS_StartPending -> "start_pending"
        SS_Running -> "running"
        SS_Paused -> "paused"
        SS_PausePending -> "pause_pending"
        SS_ContinuePending -> "continue_pending"


data SCMAccess = SCMAccess C.CULong
scmAccessRead = SCMAccess [C.pure|unsigned long{ GENERIC_READ}|]

scmAccessAll = SCMAccess [C.pure|unsigned long{SC_MANAGER_ALL_ACCESS}|]

data ServiceAccess = ServiceAccess C.CULong

serviceAccessStart = ServiceAccess [C.pure|unsigned long{SERVICE_START | SERVICE_QUERY_STATUS}|]

serviceAccessStop = ServiceAccess [C.pure|unsigned long{SERVICE_STOP | SERVICE_QUERY_STATUS}|]

serviceAccessQuery = ServiceAccess [C.pure|unsigned long{SERVICE_QUERY_STATUS}|]

serviceAccessAll = ServiceAccess [C.pure|unsigned long{SERVICE_ALL_ACCESS}|]


class (Monad m) => ServiceOpener hscm hsvc m | m -> hsvc, m-> hscm where
    openService:: hscm-> ByteString-> ServiceAccess->  m hsvc
    closeService:: hsvc -> m ()
    
class (Monad m) => ServiceReader hsvc m | m -> hsvc where
    getServiceState:: hsvc-> m ServiceState

class (Monad m, ServiceReader hsvc m) => ServiceController hscm hsvc m | m -> hsvc, m-> hscm where
    killService:: hsvc -> m ()
    stopService:: hsvc-> m ()
    startService:: hsvc-> m ()

    
class (Monad m) => ThreadDelay m where
    threadDelay:: Int -> m ()

class (Monad m) => Timeout m where
    timeout:: Int-> m a-> m (Maybe a)

class (Monad m) => SCMOpener hscm m | m -> hscm where
    openSCM:: SCMAccess-> m hscm
    closeSCM:: hscm -> m ()
