module Solid.Bind where

import Solid.Common

class Bind m r where
  bind :: (a -> r) -> m a -> r

instance Monad m => Bind m (m b) where
  bind :: (a -> m b) -> m a -> m b
  bind = (=<<)

instance Bind m r => Bind m (b -> r) where
  bind :: (a -> b -> r) -> m a -> b -> r
  bind f ma b = bind (flip f b) ma

(-<) :: Bind m r => (a -> r) -> m a -> r
(-<) = bind

infixl 1 -<
