
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Service
  where


import qualified Data.ByteString.Char8 as C8
import qualified Language.C.Inline as C

import Data.Monoid ((<>))
import Data.Either
import Data.Text.Lazy as T

import qualified Data.Aeson.Types as A
  ( toJSON
  , ToJSON
  , object
  , (.=)
  )

C.context (C.baseCtx <> C.bsCtx)

C.include "windows.h"
C.include "winsvc.h"

data ServiceState
  = SS_Stopped
  | SS_StopPending
  | SS_StartPending
  | SS_Running
  | SS_Paused
  | SS_PausePending
  | SS_ContinuePending

instance A.ToJSON ServiceState where
  toJSON state = A.object
    [ "service_state" A..= state_str
    ]
    where
      state_str:: T.Text
      state_str = case state of
        SS_Stopped -> "stopped"
        SS_StopPending -> "stop_pending"
        SS_StartPending -> "start_pending"
        SS_Running -> "running"
        SS_Paused -> "paused"
        SS_PausePending -> "pause_pending"
        SS_ContinuePending -> "continue_pending"

type Error = Text

getServiceState:: C8.ByteString-> IO (Either Error ServiceState)
getServiceState name = do
  ibool <- fmap fromIntegral
    [C.block|int
      {
        char * s_svc_name = 0;
        size_t sz_svc_name = 0;
        int ret = 0;
        sz_svc_name = $bs-len:name;
        s_svc_name = malloc( sz_svc_name + 1);
        if( !s_svc_name)
          return -1;
        s_svc_name[ sz_svc_name ] = 0;
        memcpy( s_svc_name, $bs-ptr:name, sz_svc_name);
        SC_HANDLE h_scm = 0;
        h_scm = OpenSCManager( 0, 0, GENERIC_READ);
        if( !h_scm)
        {
          ret = -2;
          goto cleanup1;
        }
        SC_HANDLE h_svc = 0;
        h_svc = OpenService( h_scm, s_svc_name, SERVICE_QUERY_STATUS);
        if( !h_svc)
        {
          ret = -3;
          goto cleanup2;
        }
        SERVICE_STATUS ss;
        memset( &ss, sizeof( ss), 0);
        if( !QueryServiceStatus( h_svc, &ss))
        {
           ret = -4;
           goto cleanup3;
        }

        ret = ss. dwCurrentState;

        cleanup3:
        CloseServiceHandle( h_svc);
        cleanup2:
        CloseServiceHandle( h_scm);
        cleanup1:
        free( s_svc_name);
        return ret;
      }
              |]
  return $ case ibool of
    (-1) -> Left "failed to allocate temp memory"
    (-2) -> Left "failed to open SC manager"
    (-3) -> Left "failed to open service"
    (-4) -> Left "failed to query service"
    1 -> Right SS_Stopped
    2 -> Right SS_StartPending
    3 -> Right SS_StopPending
    4 -> Right SS_Running
    5 -> Right SS_ContinuePending
    6 -> Right SS_PausePending
    7 -> Right SS_Paused
    _ -> Left "unsupported service status returned"

startService:: C8.ByteString-> IO (Maybe Error)
startService name = do
  iret <- fmap fromIntegral $
    [C.block| int
     {
        char * s_svc_name = 0;
        size_t sz_svc_name = 0;
        int ret = 0;
        sz_svc_name = $bs-len:name;
        s_svc_name = malloc( sz_svc_name + 1);
        if( !s_svc_name)
          return -1;
        s_svc_name[ sz_svc_name ] = 0;
        memcpy( s_svc_name, $bs-ptr:name, sz_svc_name);
        SC_HANDLE h_scm = 0;
        h_scm = OpenSCManager( 0, 0, GENERIC_READ);
        if( !h_scm)
        {
          ret = -2;
          goto cleanup1;
        }
        SC_HANDLE h_svc = 0;
        h_svc = OpenService( h_scm, s_svc_name, SERVICE_START);
        if( !h_svc)
        {
          ret = -3;
          goto cleanup2;
        }

        if( !StartService( h_svc, 0, 0))
        {
          ret = 0;
        }else
        {
          ret = 1;
        }


        cleanup3:
        CloseServiceHandle( h_svc);
        cleanup2:
        CloseServiceHandle( h_scm);
        cleanup1:
        free( s_svc_name);
        return ret;
     }
     |]
  return $ case iret of
    0    -> Just "failed to start"
    1    -> Nothing
    (-1) -> Just "failed to allocate temp memory"
    (-2) -> Just "failed to open SC manager"
    (-3) -> Just "failed to open service"
    (-4) -> Just "failed to query service"
    _    -> Just "unsupported return code"


stopService:: C8.ByteString-> IO (Maybe Error)
stopService name = do
  iret <- fmap fromIntegral $
    [C.block| int
     {
        char * s_svc_name = 0;
        size_t sz_svc_name = 0;
        int ret = 0;
        sz_svc_name = $bs-len:name;
        s_svc_name = malloc( sz_svc_name + 1);
        if( !s_svc_name)
          return -1;
        s_svc_name[ sz_svc_name ] = 0;
        memcpy( s_svc_name, $bs-ptr:name, sz_svc_name);
        SC_HANDLE h_scm = 0;
        h_scm = OpenSCManager( 0, 0, GENERIC_READ);
        if( !h_scm)
        {
          ret = -2;
          goto cleanup1;
        }
        SC_HANDLE h_svc = 0;
        h_svc = OpenService( h_scm, s_svc_name, SERVICE_STOP);
        if( !h_svc)
        {
          ret = -3;
          goto cleanup2;
        }

        SERVICE_STATUS ss;
        if( !ControlService( h_svc, SERVICE_CONTROL_STOP, &ss))
        {
          ret = 0;
        }else
        {
          ret = 1;
        }


        cleanup3:
        CloseServiceHandle( h_svc);
        cleanup2:
        CloseServiceHandle( h_scm);
        cleanup1:
        free( s_svc_name);
        return ret;
     }
     |]
  return $ case iret of
    0    -> Just "failed to stop"
    1    -> Nothing
    (-1) -> Just "failed to allocate temp memory"
    (-2) -> Just "failed to open SC manager"
    (-3) -> Just "failed to open service"
    (-4) -> Just "failed to query service"
    _    -> Just "unsupported return code"

killService
  :: C8.ByteString
  -> IO (Maybe Error)
killService bsname = C8.useAsCString bsname $ \name -> do
  iret <- worker name
  return $ case iret of
    1    -> Nothing
    0    -> Just "failed to terminate process"
    (-1) -> Just "failed to open SCM"
    (-2) -> Just "failed to open service"
    (-3) -> Just "failed to query service status"
    (-4) -> Just "failed to open service"
    where
      worker name = fromIntegral <$> do
        [C.block|int
        {
          int ret = 0;
          SC_HANDLE h_scm = 0;
          h_scm = OpenSCManager( 0, 0, GENERIC_READ);
          if( !h_scm)
          {
            ret = -1;
            goto cleanup1;
          }
          SC_HANDLE h_svc = 0;
          h_svc = OpenService( h_scm, $(char * name), SERVICE_QUERY_STATUS);
          if( !h_svc)
          {
            ret = -2;
            goto cleanup2;
          }

          SERVICE_STATUS_PROCESS ssp;
          DWORD out = 0;
          if( !QueryServiceStatusEx( h_svc, SC_STATUS_PROCESS_INFO, (LPBYTE)&ssp, sizeof( ssp), &out))
          {
            ret = -3;
            goto cleanup3;
          }

          HANDLE h_proc = 0;
          h_proc = OpenProcess( PROCESS_TERMINATE, 0, ssp. dwProcessId);
          if( !h_proc)
          {
            ret = -4;
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
          CloseServiceHandle( h_svc);
          cleanup2:
          CloseServiceHandle( h_scm);
          cleanup1:
          return ret;
        }|]

