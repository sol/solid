{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ByteString (module ByteString) where

import Solid.Common
import Solid.Exception
import Solid.String.Type (String)
import Solid.ByteString as ByteString

import Data.ByteString qualified as Haskell
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

asString :: ByteString -> Maybe String
asString (Bytes string) = if Haskell.isValidUtf8 string then Just (Bytes string) else Nothing

asString! :: HasCallStack => ByteString -> String
asString! (Bytes string) = if Haskell.isValidUtf8 string then Bytes string else throw! UnicodeDecodeError

decodeUtf8 :: ByteString -> String
decodeUtf8 input = case input.asString of
  Just xs -> xs
  Nothing -> Bytes . Text.encodeUtf8 $ Text.decodeUtf8With Text.lenientDecode (unBytes input)

pack :: [Word8] -> ByteString 
pack = Bytes . Haskell.pack

unpack :: ByteString -> [Word8]
unpack = Haskell.unpack . unBytes

instance HasField "asString" ByteString (Maybe String) where
  getField = asString

instance HasField "asString\7433" ByteString String where
  getField = asString!

instance HasField "decodeUtf8" ByteString String where
  getField = decodeUtf8

instance HasField "pack" [Word8] ByteString where
  getField = pack

instance HasField "unpack" ByteString [Word8] where
  getField = unpack
