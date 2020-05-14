-- | Generic arity curry/ uncurry
module Calamity.Internal.GenericCurry
    ( Params(..)
    , Uncurry(..) ) where

import           Data.Typeable

import           GHC.TypeNats

class Params t where
  type Parameters t
  type ParametersCollector t r

  collectParams :: t -> ParametersCollector t (Parameters t)
  applyParams :: t -> (Parameters t -> r) -> ParametersCollector t r

type family ParamsInstanceSelector t :: Nat where
  ParamsInstanceSelector (a -> b -> c -> d -> e -> f -> r) = 6
  ParamsInstanceSelector (a -> b -> c -> d -> e -> r) = 5
  ParamsInstanceSelector (a -> b -> c -> d -> r) = 4
  ParamsInstanceSelector (a -> b -> c -> r) = 3
  ParamsInstanceSelector (a -> b -> r) = 2
  ParamsInstanceSelector (a -> r) = 1

class Params' (flag :: Nat) t where
  type Parameters' (flag :: Nat) t
  type ParametersCollector' (flag :: Nat) t r

  collectParams' :: Proxy flag -> t -> ParametersCollector' flag t (Parameters' flag t)
  applyParams' :: Proxy flag -> t -> (Parameters' flag t -> r) -> ParametersCollector' flag t r

instance (ParamsInstanceSelector t ~ flag, Params' flag t) => Params t where
  type Parameters t = Parameters' (ParamsInstanceSelector t) t
  type ParametersCollector t r = ParametersCollector' (ParamsInstanceSelector t) t r

  collectParams = collectParams' (Proxy @flag)
  applyParams = applyParams' (Proxy @flag)

instance Params' 6 (a -> b -> c -> d -> e -> f -> r) where
  type Parameters' 6 (a -> b -> c -> d -> e -> f -> r) = (a, b, c, d, e, f)
  type ParametersCollector' 6 (a -> b -> c -> d -> e -> f -> r) rt = a -> b -> c -> d -> e -> f -> rt

  collectParams' _ _ = (,,,,,)
  applyParams' _ _ fn = \a b c d e f -> fn (a, b, c, d, e, f)

instance Params' 5 (a -> b -> c -> d -> e -> r) where
  type Parameters' 5 (a -> b -> c -> d -> e -> r) = (a, b, c, d, e)
  type ParametersCollector' 5 (a -> b -> c -> d -> e -> r) rt = a -> b -> c -> d -> e -> rt

  collectParams' _ _ = (,,,,)
  applyParams' _ _ fn = \a b c d e -> fn (a, b, c, d, e)

instance Params' 4 (a -> b -> c -> d -> r) where
  type Parameters' 4 (a -> b -> c -> d -> r) = (a, b, c, d)
  type ParametersCollector' 4 (a -> b -> c -> d -> r) rt = a -> b -> c -> d -> rt

  collectParams' _ _ = (,,,)
  applyParams' _ _ fn = \a b c d -> fn (a, b, c, d)

instance Params' 3 (a -> b -> c -> r) where
  type Parameters' 3 (a -> b -> c -> r) = (a, b, c)
  type ParametersCollector' 3 (a -> b -> c -> r) rt = a -> b -> c -> rt

  collectParams' _ _ = (,,)
  applyParams' _ _ fn = \a b c -> fn (a, b, c)

instance Params' 2 (a -> b -> r) where
  type Parameters' 2 (a -> b -> r) = (a, b)
  type ParametersCollector' 2 (a -> b -> r) rt = a -> b -> rt

  collectParams' _ _ = (,)
  applyParams' _ _ fn = \a b -> fn (a, b)

instance Params' 1 (a -> r) where
  type Parameters' 1 (a -> r) = a
  type ParametersCollector' 1 (a -> r) rt = a -> rt

  collectParams' _ _ = id
  applyParams' _ _ fn = \a -> fn a

class Uncurry t where
  type Uncurried t

  uncurryG :: t -> Uncurried t

class Uncurry' (flag :: Nat) t where
  type Uncurried' (flag :: Nat) t

  uncurryG' :: Proxy flag -> t -> Uncurried' flag t

instance (ParamsInstanceSelector t ~ flag, Uncurry' flag t) => Uncurry t where
  type Uncurried t = Uncurried' (ParamsInstanceSelector t) t

  uncurryG = uncurryG' (Proxy @flag)

instance Uncurry' 6 (a -> b -> c -> d -> e -> f -> r) where
  type Uncurried' 6 (a -> b -> c -> d -> e -> f -> r) = (a, b, c, d, e, f) -> r

  uncurryG' _ fn (a, b, c, d, e, f) = fn a b c d e f

instance Uncurry' 5 (a -> b -> c -> d -> e -> r) where
  type Uncurried' 5 (a -> b -> c -> d -> e -> r) = (a, b, c, d, e) -> r

  uncurryG' _ f (a, b, c, d, e) = f a b c d e

instance Uncurry' 4 (a -> b -> c -> d -> r) where
  type Uncurried' 4 (a -> b -> c -> d -> r) = (a, b, c, d) -> r

  uncurryG' _ f (a, b, c, d) = f a b c d

instance Uncurry' 3 (a -> b -> c -> r) where
  type Uncurried' 3 (a -> b -> c -> r) = (a, b, c) -> r

  uncurryG' _ f (a, b, c) = f a b c

instance Uncurry' 2 (a -> b -> r) where
  type Uncurried' 2 (a -> b -> r) = (a, b) -> r

  uncurryG' _ f (a, b) = f a b

instance Uncurry' 1 (a -> r) where
  type Uncurried' 1 (a -> r) = a -> r

  uncurryG' _ f = f
