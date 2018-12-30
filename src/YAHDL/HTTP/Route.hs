-- | The route type

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module YAHDL.HTTP.Route
  ( mkRouteBuilder
  , giveID
  , buildRoute
  , RouteBuilder
  , Route
  , SFragment(..)
  , ID(..)
  , RouteFragmentable(..)
  , RouteMethod(..)
  )
where

import           Data.Maybe                     ( fromJust )
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.List                      ( lookup )
import qualified Data.Text                     as T

import           YAHDL.Types.Snowflake
import           YAHDL.Types.General

data RouteFragment
  = SFragment' Text
  | ID' TypeRep
  deriving (Generic, Show, Eq)

newtype SFragment = SFragment Text
data ID a = ID

instance Hashable RouteFragment

data RouteMethod
  = GET
  | POST
  | DELETE
  | PUT
  deriving (Generic, Show, Eq)

instance Hashable RouteMethod

$(singletons [d|
  data RouteRequirement = NotNeeded | Required | Satisfied
    deriving (Generic, Show, Eq)
  |])

data RouteBuilder (idState :: [(Type, RouteRequirement)]) = UnsafeMkRouteBuilder
  { method  :: RouteMethod
  , route   :: [RouteFragment]
  , ids     :: [(TypeRep, Word64)]
  }

mkRouteBuilder :: RouteMethod -> RouteBuilder '[]
mkRouteBuilder method = UnsafeMkRouteBuilder method [] []

giveID
  :: forall k ids
   . Typeable k
  => RouteBuilder ids
  -> Snowflake k
  -> RouteBuilder ('(k, 'Satisfied) ': ids)
giveID (UnsafeMkRouteBuilder method route ids) (Snowflake id) =
  UnsafeMkRouteBuilder method route ((typeRep (Proxy :: Proxy k), id) : ids)

type family MyLookup (x :: k) (l :: [(k, v)]) :: Maybe v where
  MyLookup k ('(k, v) ': xs) = 'Just v
  MyLookup k ('(_, v) ': xs) = MyLookup k xs
  MyLookup _ '[]             = 'Nothing

type family MyElem (x :: k) (l :: [k]) :: Bool where
  MyElem _ '[]      = 'False
  MyElem k (k : _)  = 'True
  MyElem k (_ : xs) = MyElem k xs

type family EnsureFulfilled (ids :: [(k, RouteRequirement)]) :: Bool where
  EnsureFulfilled ids = EnsureFulfilledInner ids '[] 'True

type family EnsureFulfilledInner (ids :: [(k, RouteRequirement)]) (seen :: [k]) (ok :: Bool) :: Bool where
  EnsureFulfilledInner '[]                      _    ok = ok
  EnsureFulfilledInner ('(k, 'NotNeeded) ': xs) seen ok = EnsureFulfilledInner xs (k ': seen) ok
  EnsureFulfilledInner ('(k, 'Satisfied) ': xs) seen ok = EnsureFulfilledInner xs (k ': seen) ok
  EnsureFulfilledInner ('(k, 'Required)  ': xs) seen ok = EnsureFulfilledInner xs (k ': seen) (MyElem k seen && ok)

type family AddRequired k (ids :: [(Type, RouteRequirement)]) :: [(Type, RouteRequirement)] where
  AddRequired k ids = '(k, AddRequiredInner (MyLookup k ids)) ': ids

type family AddRequiredInner (k :: Maybe RouteRequirement) :: RouteRequirement where
  AddRequiredInner ('Just 'Required)  = 'Required
  AddRequiredInner ('Just 'Satisfied) = 'Satisfied
  AddRequiredInner ('Just 'NotNeeded) = 'Required
  AddRequiredInner 'Nothing           = 'Required

class Typeable a => RouteFragmentable a ids where
  type ConsRes a ids

  (!:!) :: a -> RouteBuilder ids -> ConsRes a ids

instance RouteFragmentable SFragment ids where
  type ConsRes SFragment ids = RouteBuilder ids

  (SFragment t) !:! (UnsafeMkRouteBuilder m r ids) =
    UnsafeMkRouteBuilder m (SFragment' t : r) ids

instance Typeable a => RouteFragmentable (ID (a :: Type)) (ids :: [(Type, RouteRequirement)]) where
  type ConsRes (ID a) ids = RouteBuilder (AddRequired a ids)

  ID !:! (UnsafeMkRouteBuilder m r ids) =
    UnsafeMkRouteBuilder m (ID' (typeRep (Proxy :: Proxy a)) : r) ids

infixr 5 !:!

data Route = Route RouteMethod [Text] Text (Maybe (Snowflake Channel)) (Maybe (Snowflake Guild))
  deriving (Generic, Show, Eq)

instance Hashable Route where
  hashWithSalt s (Route m _ ident c g) = hashWithSalt s (m, ident, c, g)

buildRoute
  :: forall (ids :: [(Type, RouteRequirement)])
   . EnsureFulfilled ids ~ 'True
  => RouteBuilder ids
  -> Route
buildRoute (UnsafeMkRouteBuilder method route ids) = Route
  method
  (map goR route)
  (T.concat (map goIdent route))
  (Snowflake <$> lookup (typeRep (Proxy :: Proxy Channel)) ids)
  (Snowflake <$> lookup (typeRep (Proxy :: Proxy Guild)) ids)
 where
  goR (SFragment' t) = t
  goR (ID'        t) = show . fromJust $ lookup t ids

  goIdent (SFragment' t) = t
  goIdent (ID'        t) = show t
