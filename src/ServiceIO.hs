{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ServiceIO where

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8
    ( ByteString
    )
import qualified Language.C.Inline as C
import qualified Foreign.C.Types as C
import qualified Control.Concurrent as C
import qualified System.Timeout as T
import Control.Monad.Catch as E
import Control.Exception as CE

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

import Types

C.context (C.baseCtx <> C.bsCtx)

C.include "windows.h"
C.include "winsvc.h"

data EServiceOpenFailed = EServiceOpenFailed
    deriving (Show)
instance Exception EServiceOpenFailed

data EServiceQueryFailed = EServiceQueryFailed
    deriving Show
instance Exception EServiceQueryFailed

data EServiceStartFailed = EServiceStartFailed
    deriving Show
instance Exception EServiceStartFailed

data EServiceStopFailed = EServiceStopFailed
    deriving Show
instance Exception EServiceStopFailed

data EProcessFailedToTerminate = EProcessFailedToTerminate
    deriving Show
instance Exception EProcessFailedToTerminate

data EProcessOpenFailed = EProcessOpenFailed
    deriving Show
instance Exception EProcessOpenFailed

data ESCMOpenFailed = ESCMOpenFailed
    deriving Show
instance Exception ESCMOpenFailed

data ServiceHandle = ServiceHandle ( Ptr ())
data SCMHandle = SCMHandle ( Ptr ())


instance ServiceOpener SCMHandle ServiceHandle IO where
    openService = ServiceIO.openService
    closeService = ServiceIO.closeService

instance ServiceReader ServiceHandle IO where
    getServiceState = ServiceIO.getServiceState
    
instance ThreadDelay IO where
    threadDelay = C.threadDelay

instance Timeout IO where
    timeout = T.timeout

instance SCMOpener SCMHandle IO where
    openSCM = ServiceIO.openSCM
    closeSCM = ServiceIO.closeSCM

instance ServiceController SCMHandle ServiceHandle IO where
    startService = ServiceIO.startService
    stopService = ServiceIO.stopService
    killService = ServiceIO.killService

openService:: SCMHandle-> ByteString-> ServiceAccess-> IO ServiceHandle    
openService (SCMHandle hSCM) name (ServiceAccess access) = C8.useAsCString name $ \cname -> do
  rawptr <- [C.block|void*
    {
      SC_HANDLE h_svc = 0;
      h_svc = OpenService( $(void *hSCM), $(char * cname), $(unsigned long access));
      return h_svc;
    }|]
  if rawptr == nullPtr
    then throwM EServiceOpenFailed
    else return $ ServiceHandle rawptr
closeService:: ServiceHandle -> IO ()    
closeService (ServiceHandle hsvc) = 
  [C.exp|void{ CloseServiceHandle($(void * hsvc))}|]

getServiceState
  :: ServiceHandle
  -> IO ServiceState
getServiceState (ServiceHandle svc) = do
  ibool <- fmap fromIntegral
    [C.block|int
      {
        SERVICE_STATUS ss;
        memset( &ss, sizeof( ss), 0);
        if( !QueryServiceStatus( $(void* svc), &ss))
        {
           return -1;
        }

        return ss. dwCurrentState;
      }
    |]
  return $ case ibool of
    (-1) -> throw EServiceQueryFailed
    1 -> SS_Stopped
    2 -> SS_StartPending
    3 -> SS_StopPending
    4 -> SS_Running
    5 -> SS_ContinuePending
    6 -> SS_PausePending
    7 -> SS_Paused

startService
  :: ServiceHandle
  -> IO ()
startService svc_@(ServiceHandle svc) = do
  state <- Types.getServiceState svc_
  if state == SS_Running
    then return ()
    else do
      iret <- fmap fromIntegral
        [C.block| int
         {
           if( !StartService( $(void* svc), 0, 0))
           {
             return 0;
           }
           return 1;
         }
         |]
      case iret of
        0    -> throwM EServiceStartFailed
        1    -> return ()

stopService
  :: ServiceHandle
  -> IO ()
stopService svc_@(ServiceHandle svc) = do
  state <- Types.getServiceState svc_
  if state == SS_Stopped
    then return ()
    else do
      iret <- fmap fromIntegral
        [C.block| int
         {
            SERVICE_STATUS ss;
            if( !ControlService( $(void *svc), SERVICE_CONTROL_STOP, &ss))
            {
              return 0;
            }
            return 1;
         }
         |]
      case iret of
        0    -> throwM EServiceStopFailed
        1    -> return ()

killService
  :: ServiceHandle
  -> IO ()
killService (ServiceHandle svc) =  do
  iret <- worker
  case iret of
    1    -> return ()
    0    -> throwM EProcessFailedToTerminate
    (-1) -> throwM EServiceQueryFailed
    (-2) -> throwM EProcessOpenFailed
    where
      worker :: IO Int
      worker = fmap fromIntegral $ do
          [C.block|int
          {
            int ret = 0;
            SERVICE_STATUS_PROCESS ssp;
            DWORD out = 0;
            if( !QueryServiceStatusEx( $(void* svc)
                 , SC_STATUS_PROCESS_INFO
                 , (LPBYTE)&ssp
                 , sizeof( ssp)
                 , &out
                 ))
            {
              ret = -1;
              goto cleanup3;
            }

            HANDLE h_proc = 0;
            h_proc = OpenProcess( PROCESS_TERMINATE, 0, ssp. dwProcessId);
            if( !h_proc)
            {
              ret = -2;
              goto cleanup3;
            }
            if( !TerminateProcess( h_proc, 0))
            {
              ret = 0;
              goto cleanup4;
            }
            ret = 1;

            cleanup4:
            CloseHandle( h_proc);
            cleanup3:
            return ret;
          }|]

openSCM:: SCMAccess-> IO SCMHandle
openSCM (SCMAccess access)= do
  rawptr <- [C.block|void*
    {
      SC_HANDLE h_scm = 0;
      h_scm = OpenSCManager( 0, 0, $(unsigned long access));
      return h_scm;
    }
    |]
  return $ if rawptr == nullPtr
    then throw ESCMOpenFailed
    else SCMHandle rawptr

closeSCM:: SCMHandle -> IO ()
closeSCM (SCMHandle hSCM) = [C.block|void
  {
    CloseServiceHandle( $(void * hSCM));
  }|]

