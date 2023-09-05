module System.Environment.Import where

import Solid
import System.Environment qualified as Haskell
import qualified Haskell

getArgs :: IO [String]
getArgs = map pack <$> Haskell.getArgs

getEnv :: String -> IO String
getEnv = fmap pack . Haskell.getEnv . unpack

getEnvironment :: IO [(String, String)]
getEnvironment = map (bimap pack pack) <$> Haskell.getEnvironment

getProgName :: IO String
getProgName = pack <$> Haskell.getProgName

lookupEnv :: String -> IO (Maybe String)
lookupEnv = fmap (fmap pack) . Haskell.lookupEnv . unpack

setEnv :: String -> String -> IO ()
setEnv name value = Haskell.setEnv name.unpack value.unpack

unsetEnv :: String -> IO ()
unsetEnv = Haskell.unsetEnv . unpack

withArgs :: [String] -> IO a -> IO a
withArgs = Haskell.withArgs . map unpack

withProgName :: String -> IO a -> IO a
withProgName = Haskell.withProgName . unpack

executablePath :: Maybe (IO (Maybe FilePath))
executablePath = (>>= maybe (return Nothing) (fmap Just . Haskell.fromFilePath)) <$> Haskell.executablePath

getExecutablePath :: IO FilePath
getExecutablePath = Haskell.getExecutablePath >>= Haskell.fromFilePath
