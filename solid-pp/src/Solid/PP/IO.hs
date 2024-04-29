module Solid.PP.IO (
  module Exports
, pass
, die
, readFile
, writeFile
, hPutStr
, encodeUtf8
, decodeUtf8
) where

import           Prelude as Exports hiding (readFile, writeFile, lines, unlines, foldl)
import           Data.List as Exports (foldl')
import           Data.List.NonEmpty as Exports (NonEmpty(..))
import           Control.Applicative as Exports
import           Control.Arrow as Exports ((&&&), (>>>))
import           Control.Monad as Exports
import           Data.Maybe as Exports
import           Data.Functor as Exports hiding (unzip)
import           Data.Bifunctor as Exports
import           Data.Foldable as Exports (for_)
import           Data.String as Exports (IsString(..))
import           Data.Proxy as Exports (Proxy(..))
import           System.IO as Exports (Handle, IOMode(..), withFile)
import           Control.Exception as Exports
import           GHC.Records as Exports (HasField(..))

import           Data.Text as Exports (Text, pack, unpack, lines, unlines)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8')
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import           System.Environment (getProgName)
import           System.Exit (exitFailure)
import           System.IO (stderr, hPutStrLn)

pass :: Monad m => m ()
pass = return ()

die :: (String -> String) -> IO a
die err = getProgName >>= hPutStrLn stderr . err >> exitFailure

readFile :: FilePath -> IO Text
readFile file = B.readFile file >>= decodeUtf8 file

writeFile :: FilePath -> Text -> IO ()
writeFile file = B.writeFile file . encodeUtf8

hPutStr :: Handle -> Text -> IO ()
hPutStr h = B.hPutStr h . encodeUtf8

decodeUtf8 :: FilePath -> ByteString -> IO Text
decodeUtf8 file = either (const err) return . decodeUtf8'
  where
    err = throwIO (ErrorCall $ "Encountered an invalid UTF-8 sequence while reading the file " <> show file <> ".")
