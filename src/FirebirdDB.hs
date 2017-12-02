module FirebirdDB where

import qualified Data.Conduit as C

import           Data.Conduit
  ( Conduit
  , Sink
  , Source
  )

import qualified Data.ByteString.Char8 as BS

isFDBHeader
  :: BS.ByteString
  -> Bool
isFDBHeader str = header == (BS.take (BS.length header) str)
    where 
      header = BS.pack "\x01\x00\x39\x30\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

isISOHeader
  :: BS.ByteString
  -> Bool
isISOHeader str = header == (BS.take (BS.length header) str)
    where 
      header = BS.pack "\x33\xed\x90\x90\x90\x90\x90\x90\x90\x90\x90\x90\x90\x90\x90\x90"

checkHeaderC :: Monad m => Sink BS.ByteString m Bool
checkHeaderC = do
  mstr <- C.await
  case mstr of
    Nothing -> return False
    Just str -> do
      C.leftover str
      return $ isISOHeader str

