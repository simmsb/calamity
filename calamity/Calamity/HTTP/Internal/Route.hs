{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | The route type
 Why I did this I don't know
-}
module Calamity.HTTP.Internal.Route (
  mkRouteBuilder,
  giveID,
  giveParam,
  buildRoute,
  routeKey,
  RouteKey,
  RouteBuilder,
  RouteRequirement,
  Route (path),
  S (..),
  PS (..),
  ID (..),
  RouteFragmentable (..),
) where

import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Snowflake
import Data.Hashable
import Data.Kind
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Req
import Optics.TH
import qualified TextShow

data RouteFragment
  = -- | Static string fragment
    S' Text
  | -- | Parameterised string fragment
    PS' String
  | -- | ID fragment
    ID' TypeRep
  deriving (Generic, Show, Eq)

-- | A static string fragment of a route
newtype S = S Text

-- | A parameterised string fragment of a route
data PS (s :: Symbol) = PS

-- | An id fragment of a route
data ID a = ID

instance Hashable RouteFragment

data RouteRequirement
  = NotNeeded
  | Required
  | Satisfied
  deriving (Show, Eq)

data RequirementType
  = IDRequirement Type
  | PSRequirement Symbol

data RouteBuilder (reqstate :: [(RequirementType, RouteRequirement)]) = UnsafeMkRouteBuilder
  { route :: [RouteFragment]
  , ids :: [(TypeRep, Word64)]
  , params :: [(String, Text)]
  }

mkRouteBuilder :: RouteBuilder '[]
mkRouteBuilder = UnsafeMkRouteBuilder [] [] []

giveID ::
  forall t reqs.
  Typeable t =>
  Snowflake t ->
  RouteBuilder reqs ->
  RouteBuilder ('( 'IDRequirement t, 'Satisfied) ': reqs)
giveID (Snowflake id) (UnsafeMkRouteBuilder route ids params) =
  UnsafeMkRouteBuilder route ((typeRep $ Proxy @t, id) : ids) params

giveParam ::
  forall (s :: Symbol) reqs.
  KnownSymbol s =>
  Text ->
  RouteBuilder reqs ->
  RouteBuilder ('( 'PSRequirement s, 'Satisfied) ': reqs)
giveParam value (UnsafeMkRouteBuilder route ids params) =
  UnsafeMkRouteBuilder route ids ((symbolVal $ Proxy @s, value) : params)

type family (&&) (a :: Bool) (b :: Bool) :: Bool where
  'True && 'True = 'True
  _ && _ = 'False

type family Lookup (x :: k) (l :: [(k, v)]) :: Maybe v where
  Lookup k ('(k, v) ': xs) = 'Just v
  Lookup k ('(_, v) ': xs) = Lookup k xs
  Lookup _ '[] = 'Nothing

type family IsElem (x :: k) (l :: [k]) :: Bool where
  IsElem _ '[] = 'False
  IsElem k (k : _) = 'True
  IsElem k (_ : xs) = IsElem k xs

type family EnsureFulfilled (reqs :: [(RequirementType, RouteRequirement)]) :: Constraint where
  EnsureFulfilled reqs = EnsureFulfilledInner reqs '[] 'True

type family EnsureFulfilledInner (reqs :: [(RequirementType, RouteRequirement)]) (seen :: [RequirementType]) (ok :: Bool) :: Constraint where
  EnsureFulfilledInner '[] _ 'True = ()
  EnsureFulfilledInner ('(k, 'NotNeeded) ': xs) seen ok = EnsureFulfilledInner xs (k ': seen) ok
  EnsureFulfilledInner ('(k, 'Satisfied) ': xs) seen ok = EnsureFulfilledInner xs (k ': seen) ok
  EnsureFulfilledInner ('(k, 'Required) ': xs) seen ok = EnsureFulfilledInner xs (k ': seen) (IsElem k seen && ok)

type family AddRequired k (reqs :: [(RequirementType, RouteRequirement)]) :: [(RequirementType, RouteRequirement)] where
  AddRequired k reqs = '(k, AddRequiredInner (Lookup k reqs)) ': reqs

type family AddRequiredInner (k :: Maybe RouteRequirement) :: RouteRequirement where
  AddRequiredInner ( 'Just 'Required) = 'Required
  AddRequiredInner ( 'Just 'Satisfied) = 'Satisfied
  AddRequiredInner ( 'Just 'NotNeeded) = 'Required
  AddRequiredInner 'Nothing = 'Required

class Typeable a => RouteFragmentable a reqs where
  type ConsRes a reqs

  (//) :: RouteBuilder reqs -> a -> ConsRes a reqs

instance RouteFragmentable S reqs where
  type ConsRes S reqs = RouteBuilder reqs

  (UnsafeMkRouteBuilder r ids params) // (S t) =
    UnsafeMkRouteBuilder (r <> [S' t]) ids params

instance Typeable a => RouteFragmentable (ID (a :: Type)) (reqs :: [(RequirementType, RouteRequirement)]) where
  type ConsRes (ID a) reqs = RouteBuilder (AddRequired ( 'IDRequirement a) reqs)

  (UnsafeMkRouteBuilder r ids params) // ID =
    UnsafeMkRouteBuilder (r <> [ID' $ typeRep $ Proxy @a]) ids params

instance KnownSymbol s => RouteFragmentable (PS s) (reqs :: [(RequirementType, RouteRequirement)]) where
  type ConsRes (PS s) reqs = RouteBuilder (AddRequired ( 'PSRequirement s) reqs)

  (UnsafeMkRouteBuilder r ids params) // PS =
    UnsafeMkRouteBuilder (r <> [PS' $ symbolVal $ Proxy @s]) ids params

infixl 5 //

data Route = Route
  { path :: Url 'Https
  , key :: Text
  , channelID :: Maybe (Snowflake Channel)
  , guildID :: Maybe (Snowflake Guild)
  }
  deriving (Show)

type RouteKey = (Text, Maybe (Snowflake Channel), Maybe (Snowflake Guild))

routeKey :: Route -> RouteKey
routeKey Route {key, channelID, guildID} = (key, channelID, guildID)

baseURL :: Url 'Https
baseURL = https "discord.com" /: "api" /: "v10"

buildRoute ::
  forall (reqs :: [(RequirementType, RouteRequirement)]).
  EnsureFulfilled reqs =>
  RouteBuilder reqs ->
  Route
buildRoute (UnsafeMkRouteBuilder route ids params) =
  Route
    (foldl' (/:) baseURL $ map goR route)
    (T.concat (map goIdent route))
    (Snowflake <$> lookup (typeRep (Proxy @Channel)) ids)
    (Snowflake <$> lookup (typeRep (Proxy @Guild)) ids)
  where
    goR (S' t) = t
    goR (PS' t) = fromJust $ lookup t params
    goR (ID' t) = TextShow.showt . fromJust $ lookup t ids

    goIdent (S' t) = t
    goIdent (PS' s) = T.pack s
    goIdent (ID' t) = TextShow.showt t

$(makeFieldLabelsNoPrefix ''Route)
