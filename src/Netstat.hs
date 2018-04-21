{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Netstat
    where

import qualified Language.C.Inline as C
import Control.Monad.Catch as E
import qualified Control.Monad.Trans.Either as E
  ( left
  , right
  , runEitherT
  )
import Control.Monad.Trans.Either
  ( EitherT
  , hoistEither
  )
import Data.Monoid ((<>))
import Data.Text.Lazy as T
import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Types

C.context (C.baseCtx <> C.bsCtx)

C.include "winsock2.h"
C.include "ws2tcpip.h"
C.include "iphlpapi.h"

data ETCPTableQuery = ETCPTableQuery
    deriving Show
instance Exception ETCPTableQuery

isTcpPortListening
    :: Int
    -> IO Bool
isTcpPortListening port' = do
    ret <- worker
    case () of
        _ | ret < 0 -> throwM ETCPTableQuery
        _ | ret == [C.pure|int{MIB_TCP_STATE_LISTEN}|] -> return True
        _ -> return False
    where
        port = fromIntegral port'
        worker :: IO C.CInt
        worker = do
            [C.block|int
            {
                #define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x))
                #define FREE(x) HeapFree(GetProcessHeap(), 0, (x))
                
                PMIB_TCPTABLE pTcpTable;
                DWORD dwSize = 0;
                DWORD dwRetVal = 0;

                char szLocalAddr[128];
                char szRemoteAddr[128];

                struct in_addr IpAddr;

                int i = 0, ret = 0;

                pTcpTable = (MIB_TCPTABLE *) MALLOC(sizeof (MIB_TCPTABLE));
                if (pTcpTable == NULL) {
                    return -1;
                }

                dwSize = sizeof (MIB_TCPTABLE);
            // Make an initial call to GetTcpTable to
            // get the necessary size into the dwSize variable
                if ((dwRetVal = GetTcpTable(pTcpTable, &dwSize, TRUE)) ==
                    ERROR_INSUFFICIENT_BUFFER) {
                    FREE(pTcpTable);
                    pTcpTable = (MIB_TCPTABLE *) MALLOC(dwSize);
                    if (pTcpTable == NULL) {
                        return -1;
                    }
                }
            // Make a second call to GetTcpTable to get
            // the actual data we require
                if ((dwRetVal = GetTcpTable(pTcpTable, &dwSize, TRUE)) == NO_ERROR) {
                    for (i = 0; i < (int) pTcpTable->dwNumEntries; i++) {
                        if( ntohs((u_short)pTcpTable->table[i].dwLocalPort) == $(int port))
                        {
                            ret = pTcpTable->table[i].dwState;
                            break;
                        }
                    }
                } else {
                    FREE(pTcpTable);
                    return -1;
                }

                if (pTcpTable != NULL) {
                    FREE(pTcpTable);
                    pTcpTable = NULL;
                }    

                return ret;    
            }|]
