module Solid.DriverSpec (spec) where

import           Helper

import           System.IO.Silently
import           System.Exit
import qualified Env

import           Solid.Driver

withUnsetEnv :: String -> IO a -> IO a
withUnsetEnv name action = bracket (Env.get name) (maybe pass (Env.set name)) $ \ _ -> do
  Env.unset name
  action

spec :: Spec
spec = do
  describe "solid" $ do
    it "runs a script" $ do
      withUnsetEnv "GHC_ENVIRONMENT" $ do
        inTempDirectory $ do
          writeFile "main.hs" $ unlines [
              "import System.Exit"
            , ""
            , "name :: String"
            , "name = \"Joe\""
            , ""
            , "main :: IO ()"
            , "main = do"
            , "  stdout.writeLine \"Hey {name} 👋\""
            , "  exitWith (ExitFailure 23)"
            ]
          capture_ (solid "solid" ["main.hs"] `shouldThrow` ExitFailure 23) `shouldReturn` "Hey Joe 👋\n"
