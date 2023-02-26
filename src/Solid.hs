{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid (module Solid) where

import Solid.Common as Solid
import Solid.Exception as Solid
import Solid.IO as Solid
import Solid.Char ()
import Solid.String as Solid
import Solid.MD5 as Solid
import Solid.FilePath as Solid
import Solid.ToString as Solid

instance HasField "map" [a] ((a -> b) -> [b]) => HasField "map" [a] ((a -> b) -> [b]) where
  getField xs f = map f xs
