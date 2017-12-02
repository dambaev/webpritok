
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
import qualified Control.Exception as E
  ( bracket
  )

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

import qualified Data.Aeson.Types as A
  ( toJSON
  , ToJSON
  , object
  , (.=)
  )

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Control.Monad.Trans.Either
  ( EitherT
  , hoistEither
  )
import qualified Control.Monad.Trans.Either as E
  ( left
  , right
  , runEitherT
  )

import Types

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
  deriving Eq

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

data ServiceHandle = ServiceHandle ( Ptr ())
data ServiceAccess = ServiceAccess C.CULong
data SCMHandle = SCMHandle ( Ptr ())

data SCMAccess = SCMAccess C.CULong

scmAccessRead = SCMAccess [C.pure|unsigned long{ GENERIC_READ}|]

scmAccessAll = SCMAccess [C.pure|unsigned long{SC_MANAGER_ALL_ACCESS}|]

serviceAccessStart = ServiceAccess [C.pure|unsigned long{SERVICE_START | SERVICE_QUERY_STATUS}|]

serviceAccessStop = ServiceAccess [C.pure|unsigned long{SERVICE_STOP | SERVICE_QUERY_STATUS}|]

serviceAccessQuery = ServiceAccess [C.pure|unsigned long{SERVICE_QUERY_STATUS}|]

serviceAccessAll = ServiceAccess [C.pure|unsigned long{SERVICE_ALL_ACCESS}|]

getServiceState
  :: ServiceHandle
  -> EitherT Error IO ServiceState
getServiceState (ServiceHandle svc) = do
  ibool <- fmap fromIntegral $ liftIO
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
  case ibool of
    (-1) -> E.left "failed to query service"
    1 -> E.right SS_Stopped
    2 -> E.right SS_StartPending
    3 -> E.right SS_StopPending
    4 -> E.right SS_Running
    5 -> E.right SS_ContinuePending
    6 -> E.right SS_PausePending
    7 -> E.right SS_Paused
    _ -> E.left "unsupported service status returned"

startService
  :: ServiceHandle
  -> EitherT Error IO ()
startService svc_@(ServiceHandle svc) = do
  state <- getServiceState svc_
  if state == SS_Running
    then return ()
    else do
      iret <- fmap fromIntegral $ liftIO
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
        0    -> E.left "failed to start"
        1    -> E.right ()
        _    -> E.left "unsupported return code"


stopService
  :: ServiceHandle
  -> EitherT Error IO ()
stopService svc_@(ServiceHandle svc) = do
  state <- getServiceState svc_
  if state == SS_Stopped
    then return ()
    else do
      iret <- fmap fromIntegral $ liftIO
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
        0    -> E.left "failed to stop"
        1    -> E.right ()
        some -> E.left $ "unexpected return code " `T.append` (T.pack $ show some)



killServiceW
  :: C8.ByteString
  -> SCMHandle
  -> EitherT Error IO ()
killServiceW name scm = do
  withService scm name serviceAccessQuery killService

killService
  :: ServiceHandle
  -> EitherT Error IO ()
killService (ServiceHandle svc) =  do
  iret <- liftIO $ worker
  case iret of
    1    -> E.right ()
    0    -> E.left "failed to terminate process"
    (-1) -> E.left "failed to query service status"
    (-2) -> E.left "failed to open process"
    some -> E.left $ "unexpected return code " `T.append` (T.pack $ show some)
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

withSCM
  :: SCMAccess
  -> ( SCMHandle
     -> EitherT Error IO a
     )
  -> EitherT Error IO a
withSCM (SCMAccess access) inner = do
  ret <- liftIO $ E.bracket openSCM closeSCM wrapper
  case ret of
    Left some -> E.left some
    Right some -> E.right some
  where
    openSCM:: IO (Either Error SCMHandle)
    openSCM = do
      rawptr <- [C.block|void*
        {
          SC_HANDLE h_scm = 0;
          h_scm = OpenSCManager( 0, 0, $(unsigned long access));
          return h_scm;
        }
        |]
      return $ if rawptr == nullPtr
        then Left "failed to open scm"
        else Right $ SCMHandle rawptr
    closeSCM:: Either Error SCMHandle -> IO ()
    closeSCM (Left _) = return ()
    closeSCM (Right (SCMHandle hSCM)) = [C.block|void
      {
        CloseServiceHandle( $(void * hSCM));
      }|]
    wrapper (Left some) = return $ Left some
    wrapper (Right hSCM) = E.runEitherT $ inner hSCM

withService
  :: SCMHandle
  -> C8.ByteString
  -> ServiceAccess
  -> ( ServiceHandle
     -> EitherT Error IO a
     )
  -> EitherT Error IO a
withService (SCMHandle hSCM) name (ServiceAccess access) inner = do
  ret <- liftIO $ E.bracket openService closeService wrapper
  case ret of
    Left some -> E.left some
    Right some -> E.right some
  where
    openService = C8.useAsCString name $ \cname -> do
      rawptr <- [C.block|void*
        {
          SC_HANDLE h_svc = 0;
          h_svc = OpenService( $(void *hSCM), $(char * cname), $(unsigned long access));
          return h_svc;
        }|]
      return $ if rawptr == nullPtr
        then Left "openService failed"
        else Right $ ServiceHandle rawptr
    closeService (Left _ ) = return ()
    closeService (Right (ServiceHandle hsvc)) = 
      [C.exp|void{ CloseServiceHandle($(void * hsvc))}|]
    wrapper (Left some ) = return $ Left some
    wrapper (Right hsvc) = E.runEitherT $ inner hsvc

waitForSvcStatus
  :: ServiceHandle
  -> ServiceState
  -> EitherT Error IO ()
waitForSvcStatus svc desiredState = do
  state <- getServiceState svc
  if state /= desiredState
    then do
      liftIO $ C.threadDelay 200000
      waitForSvcStatus svc desiredState
    else return ()

startServiceSyncW
  :: SCMHandle
  -> C8.ByteString
  -> EitherT Error IO ()
startServiceSyncW scm name = withService scm name serviceAccessStart $ \svc -> 
  do
    startService svc
    waitForSvcStatus svc SS_Running

stopServiceSync
  :: ServiceHandle
  -> EitherT Error IO ()
stopServiceSync svc = do
  stopService svc
  waitForSvcStatus svc SS_Stopped

stopServiceSyncW
  :: SCMHandle
  -> C8.ByteString
  -> EitherT Error IO ()
stopServiceSyncW scm name = withService scm name serviceAccessStop stopServiceSync

getServiceStateW
  :: SCMHandle
  -> C8.ByteString
  -> EitherT Error IO ServiceState
getServiceStateW scm name = withService scm name serviceAccessQuery getServiceState


after
  :: Int
  -> EitherT Error IO a
  -> EitherT Error IO a
  -> EitherT Error IO a
after delay payload timeoutWorker = do
  mret <- liftIO $ T.timeout delay 
    (E.runEitherT payload)
  case mret of
    Nothing -> timeoutWorker
    Just (Left ret) -> E.left ret
    Just (Right ret) -> E.right ret

