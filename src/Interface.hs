{-# LANGUAGE OverloadedStrings #-}
module Interface where

import Data.ByteString.Char8
import Data.Binary.Builder

class (Monad m) => UsesHTTPFileResponse m where
  downloadFile
    :: ByteString
    -> (Builder-> IO ())
    -> IO ()
    -> m ()


class (Monad m) => ForksChild m where
  runProgram:: ByteString-> [ByteString]-> m ()

class (Monad m) => ReadsFile m where
  doesFileExist:: ByteString-> m Bool

class (Monad m) => DeletesFile m where
  removeFile:: ByteString-> m ()
