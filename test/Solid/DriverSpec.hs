{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.DriverSpec (spec) where

import           Helper

import           System.Exit

import           Solid.Driver

withUnsetEnv :: String -> IO a -> IO a
withUnsetEnv name action = bracket (Env.get name) (maybe pass (Env.set name)) $ \ _ -> do
  Env.unset name
  action

script :: String
script = unlines [
    "import System.Exit"
  , ""
  , "name :: String"
  , "-- |"
  , "--"
  , "-- >>> putStrLn \"Hey \{name} 👋\""
  , "-- Hey Joe 👋"
  , "name = \"Joe\""
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  stdout.writeLine \"Hey \{name} 👋\""
  , "  stdout.writeLine \"length: \{String.length name}\""
  , "  stdout.writeLine \"length: \{name.length}\""
  , "  exitWith (ExitFailure 23)"
  ]

spec :: Spec
spec = around_ (inTempDirectory . withUnsetEnv "GHC_ENVIRONMENT") $ do
  describe "solid" $ do
    it "runs a script" $ do
      writeFile "main.hs" script
      capture_ (solid Run "solid" ["main.hs"] `shouldThrow` ExitFailure 23) `shouldReturn` unlines [
          "Hey Joe 👋"
        , "length: 3"
        , "length: 3"
        ]

    describe "doctest" $ do
      it "runs doctests" $ do
        writeFile "main.hs" script
        hCapture_ [stderr] (solid Doctest "solid" ["main.hs"]) `shouldReturn` unlines [
            "Examples: 1  Tried: 1  Errors: 0  Failures: 0"
          ]
