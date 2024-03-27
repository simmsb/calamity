-- | Get the constructor name of something
module Calamity.Internal.ConstructorName (
  CtorName (..),
  GCtorName (..),
) where

import GHC.Generics

class GCtorName f where
  gctorName :: f a -> String

instance (Constructor c) => GCtorName (C1 c f) where
  gctorName = conName

instance (GCtorName f) => GCtorName (D1 d f) where
  gctorName (M1 a) = gctorName a

instance (GCtorName f, GCtorName g) => GCtorName (f :+: g) where
  gctorName (L1 a) = gctorName a
  gctorName (R1 a) = gctorName a

class CtorName a where
  ctorName :: a -> String
  default ctorName :: (Generic a, GCtorName (Rep a)) => a -> String
  ctorName = gctorName . from
