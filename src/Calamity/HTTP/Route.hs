-- | The route type
-- Why I did this I don't know

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Calamity.HTTP.Route
  ( mkRouteBuilder
  , giveID
  , buildRoute
  , RouteBuilder
  , Route(path)
  , S(..)
  , ID(..)
  , RouteFragmentable(..)
  )
where

import           Data.Maybe                     ( fromJust )
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.List                      ( lookup )
import qualified Data.Text                     as T

import           Calamity.Types.Snowflake
import           Calamity.Types.General

data RouteFragment
  = S' Text
  | ID' TypeRep
  deriving (Generic, Show, Eq)

newtype S = S Text
data ID a = ID

instance Hashable RouteFragment

$(singletons [d|
  data RouteRequirement = NotNeeded | Required | Satisfied
    deriving (Generic, Show, Eq)
  |])

data RouteBuilder (idState :: [(Type, RouteRequirement)]) = UnsafeMkRouteBuilder
  { route   :: [RouteFragment]
  , ids     :: [(TypeRep, Word64)]
  }

mkRouteBuilder :: RouteBuilder '[]
mkRouteBuilder = UnsafeMkRouteBuilder [] []

giveID
  :: forall k ids
   . Typeable k
  => Snowflake k
  -> RouteBuilder ids
  -> RouteBuilder ('(k, 'Satisfied) ': ids)
giveID (Snowflake id) (UnsafeMkRouteBuilder route ids) =
  UnsafeMkRouteBuilder route ((typeRep (Proxy @k), id) : ids)

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

  (!:!) :: RouteBuilder ids -> a -> ConsRes a ids

instance RouteFragmentable S ids where
  type ConsRes S ids = RouteBuilder ids

  (UnsafeMkRouteBuilder r ids) !:! (S t) =
    UnsafeMkRouteBuilder (S' t : r) ids

instance Typeable a => RouteFragmentable (ID (a :: Type)) (ids :: [(Type, RouteRequirement)]) where
  type ConsRes (ID a) ids = RouteBuilder (AddRequired a ids)

  (UnsafeMkRouteBuilder r ids) !:! ID =
    UnsafeMkRouteBuilder (ID' (typeRep (Proxy @a)) : r) ids

infixl 5 !:!

data Route = Route
  { path      :: Text
  , key       :: Text
  , channelID :: Maybe (Snowflake Channel)
  , guildID   :: Maybe (Snowflake Guild)
  } deriving (Generic, Show, Eq)

instance Hashable Route where
  hashWithSalt s (Route _ ident c g) = hashWithSalt s (ident, c, g)

baseURL :: Text
baseURL = "https://discordapp.com/api/v7"

buildRoute
  :: forall (ids :: [(Type, RouteRequirement)])
   . EnsureFulfilled ids ~ 'True
  => RouteBuilder ids
  -> Route
buildRoute (UnsafeMkRouteBuilder route ids) = Route
  (T.intercalate "/" (baseURL : map goR route'))
  (T.concat (map goIdent route'))
  (Snowflake <$> lookup (typeRep (Proxy @Channel)) ids)
  (Snowflake <$> lookup (typeRep (Proxy @Guild)) ids)
 where
  route' = reverse route

  goR (S'  t) = t
  goR (ID' t) = show . fromJust $ lookup t ids

  goIdent (S'  t) = t
  goIdent (ID' t) = show t
