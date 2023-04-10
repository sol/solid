{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.MD5 (Fingerprint) where

import           Solid.Common
import           Solid.Types
import           Solid.ToString
import           Solid.FilePath

import           GHC.Fingerprint
import           GHC.Ptr (castPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

instance ToString Fingerprint

instance HasField "toString" Fingerprint String where
  getField = toString

-- from ghc-9.4.2:GHC.Utils.Fingerprint
fingerprintByteString :: BS.ByteString -> Fingerprint
fingerprintByteString bs = unsafeDupablePerformIO $
  BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> fingerprintData (castPtr ptr) len

instance HasField "md5sum" (Bytes a) Fingerprint where
  getField = fingerprintByteString . unBytes

instance HasField "md5sum" [Fingerprint] Fingerprint where
  getField = fingerprintFingerprints

instance HasField "md5sum" FilePath (IO Fingerprint) where
  getField = getFileHash . unFilePath
