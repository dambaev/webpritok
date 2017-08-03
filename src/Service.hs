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

C.include "windows.h"
C.include "winsvc.h"

isServiceRunning:: C8.ByteString-> S.ActionM Bool
isServiceRunning name = do
  ibool <- liftIO $ fmap fromIntegral
    [C.block|int
      {
        SC_HANDLE h_svc = 0;
        h_svc = OpenSCManager( 0, 0, GENERIC_READ);
        if( !h_svc)
        {
          return 0;
        }
        CloseServiceHandle( h_svc);
        return 1;
      }
              |]
  if ibool == 0
    then return False
    else return True
