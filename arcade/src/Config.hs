module Config where

import Prelude ()
import HaskellPrelude

import           System.Directory
import           Data.Aeson
import qualified Data.Yaml as Yaml

hiedir :: FilePath
hiedir =  "hie"

data Config = Config {
  configSourceDirs :: [FilePath]
} deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \ o -> Config
    <$> o .: "source-dirs"

defaultConfig :: Config
defaultConfig = Config {
  configSourceDirs = ["src", "test"]
}

loadConfig :: IO Config
loadConfig = do
  doesFileExist file >>= \ case
    True -> Yaml.decodeFileThrow file
    False -> return defaultConfig
  where
    file = "ide.yaml"
