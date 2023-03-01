module Solid.Common (
  module Imports
, pass
) where

import           Prelude as Imports hiding (FilePath, String, lines, unlines, print, readFile, writeFile, error)
import           Data.String as Imports (IsString(..))
import           GHC.Records as Imports (HasField(..))
import           GHC.Generics as Imports (Generic)
import           Control.Monad as Imports (when, unless)

pass :: Applicative m => m ()
pass = pure ()
