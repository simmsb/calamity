-- | Generic arity curry/ uncurry
module Calamity.Internal.GenericCurry
    ( Curry(..)
    , Uncurry(..) ) where

import           Data.Typeable

import           GHC.TypeNats

class Curry t where
  type Curried t
  type Parameters t

  curryG :: t -> Curried t

type family CurryInstanceSelector t :: Nat where
  CurryInstanceSelector (a -> b -> c -> d -> e -> f -> r) = 6
  CurryInstanceSelector (a -> b -> c -> d -> e -> r) = 5
  CurryInstanceSelector (a -> b -> c -> d -> r) = 4
  CurryInstanceSelector (a -> b -> c -> r) = 3
  CurryInstanceSelector (a -> b -> r) = 2
  CurryInstanceSelector (a -> r) = 1

class Curry' (flag :: Nat) t where
  type Curried' (flag :: Nat) t
  type Parameters' (flag :: Nat) t

  curryG' :: Proxy flag -> t -> Curried' flag t

instance (CurryInstanceSelector t ~ flag, Curry' flag t) => Curry t where
  type Curried t = Curried' (CurryInstanceSelector t) t
  type Parameters t = Parameters' (CurryInstanceSelector t) t

  curryG = curryG' (Proxy @flag)

instance Curry' 6 ((a, b, c, d, e, f) -> r) where
  type Curried' 6 ((a, b, c, d, e, f) -> r) = a -> b -> c -> d -> e -> f -> r
  type Parameters' 6 ((a, b, c, d, e, f) -> r) = (a, b, c, d, e, f)

  curryG' _ fn a b c d e f = fn (a, b, c, d, e, f)

instance Curry' 5 ((a, b, c, d, e) -> r) where
  type Curried' 5 ((a, b, c, d, e) -> r) = a -> b -> c -> d -> e -> r
  type Parameters' 5 ((a, b, c, d, e) -> r) = (a, b, c, d, e)

  curryG' _ fn a b c d e = fn (a, b, c, d, e)

instance Curry' 4 ((a, b, c, d) -> r) where
  type Curried' 4 ((a, b, c, d) -> r) = a -> b -> c -> d -> r
  type Parameters' 4 ((a, b, c, d) -> r) = (a, b, c, d)

  curryG' _ fn a b c d = fn (a, b, c, d)

instance Curry' 3 ((a, b, c) -> r) where
  type Curried' 3 ((a, b, c) -> r) = a -> b -> c -> r
  type Parameters' 3 ((a, b, c) -> r) = (a, b, c)

  curryG' _ fn a b c = fn (a, b, c)

instance Curry' 2 ((a, b) -> r) where
  type Curried' 2 ((a, b) -> r) = a -> b -> r
  type Parameters' 2 ((a, b) -> r) = (a, b)

  curryG' _ fn a b = fn (a, b)

instance Curry' 1 (a -> r) where
  type Curried' 1 (a -> r) = a -> r
  type Parameters' 1 (a -> r) = a

  curryG' _ fn = fn

class Uncurry t where
  type Uncurried t

  uncurryG :: t -> Uncurried t

class Uncurry' (flag :: Nat) t where
  type Uncurried' (flag :: Nat) t

  uncurryG' :: Proxy flag -> t -> Uncurried' flag t

instance (CurryInstanceSelector t ~ flag, Uncurry' flag t) => Uncurry t where
  type Uncurried t = Uncurried' (CurryInstanceSelector t) t

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
