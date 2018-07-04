{-# LANGUAGE OverloadedStrings #-}
module Production where

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Zlib as C
import Data.Binary.Builder
import qualified Network.Wai.Conduit as C
  ( sourceRequestBody
  )
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Builder as B
  ( fromByteString
  )
import Control.Monad.IO.Class
import           Data.Conduit
  ( ($$)
  , ($$+)
  , ($$+-)
  , (=$$+)
  , (=$$+-)
  , (=$)
  , Conduit
  , Sink
  , awaitForever
  )
import qualified System.IO as IO
import qualified System.Directory as IO
import Interface

instance UsesHTTPFileResponse IO where
  downloadFile file write flush = R.runResourceT $ C.sourceFile (C8.unpack file) $$ C.gzip =$ streamConsumer 
    where
      streamConsumer = awaitForever $ \buff -> liftIO $ do
        write $ B.fromByteString buff
        flush

instance DeletesFile IO where
  removeFile file = IO.removeFile (C8.unpack file)

instance ReadsFile IO where
  doesFileExist file = IO.doesFileExist (C8.unpack file)

instance ForksChild IO where
  runProgram = undefined
