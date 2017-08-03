{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Service
  where

import qualified Web.Scotty as S
import Control.Monad.IO.Class
  ( liftIO
  )

import qualified Data.ByteString.Char8 as C8
import qualified Language.C.Inline as C

import qualified Web.Scotty as S

C.context (C.baseCtx <> C.bsCtx)

C.include "windows.h"
C.include "winsvc.h"

isServiceRunning:: C8.ByteString-> S.ActionM Bool
isServiceRunning name = do
  ibool <- liftIO $ fmap fromIntegral
    [C.block|int
      {
        char * s_svc_name = 0;
        size_t sz_svc_name = 0;
        int ret = 0;
        sz_svc_name = $bs-len:name;
        s_svc_name = malloc( sz_svc_name + 1);
        if( !s_svc_name)
          return 0;
        s_svc_name[ sz_svc_name ] = 0;
        memcpy( s_svc_name, $bs-ptr:name, sz_svc_name);
        SC_HANDLE h_scm = 0;
        h_scm = OpenSCManager( 0, 0, GENERIC_READ);
        if( !h_svc)
        {
          goto cleanup1;
        }
        SC_HANDLE h_svc = 0;
        h_svc = OpenService( h_scm, s_svc_name, GENERIC_READ);
        if( !h_svc)
        {
          goto cleanup2;
        }


        cleanup2:
        CloseServiceHandle( h_svc);
        cleanup1:
        free( s_svc_name);
        return ret;
      }
              |]
  if ibool == 0
    then return False
    else return True
