{-# LANGUAGE TemplateHaskell #-}

-- | User endpoints
module Calamity.HTTP.User (
  UserRequest (..),
  ModifyUserData (..),
  GetCurrentUserGuildsOptions (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.?=))
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import qualified Data.Aeson as Aeson
import Data.Default.Class
import Data.Function ((&))
import Data.Text (Text)
import Optics.TH
import Network.HTTP.Req

data ModifyUserData = ModifyUserData
  { username :: Maybe Text
  , -- | The avatar field should be in discord's image data format: https://discord.com/developers/docs/reference#image-data
    avatar :: Maybe Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ModifyUserData

instance CalamityToJSON' ModifyUserData where
  toPairs ModifyUserData {..} =
    [ "username" .?= username
    , "avatar" .?= avatar
    ]

instance Default ModifyUserData where
  def = ModifyUserData Nothing Nothing

data GetCurrentUserGuildsOptions = GetCurrentUserGuildsOptions
  { before :: Maybe (Snowflake Guild)
  , after :: Maybe (Snowflake Guild)
  , limit :: Maybe Integer
  }
  deriving (Show)

instance Default GetCurrentUserGuildsOptions where
  def = GetCurrentUserGuildsOptions Nothing Nothing Nothing

data UserRequest a where
  GetCurrentUser :: UserRequest User
  GetUser :: HasID User u => u -> UserRequest User
  ModifyCurrentUser :: ModifyUserData -> UserRequest User
  GetCurrentUserGuilds :: GetCurrentUserGuildsOptions -> UserRequest [Partial Guild]
  LeaveGuild :: HasID Guild g => g -> UserRequest ()
  CreateDM :: HasID User u => u -> UserRequest DMChannel

baseRoute :: RouteBuilder _
baseRoute = mkRouteBuilder // S "users" // S "@me"

instance Request (UserRequest a) where
  type Result (UserRequest a) = a

  route GetCurrentUser =
    baseRoute
      & buildRoute
  route (GetUser (getID @User -> uid)) =
    mkRouteBuilder // S "users" // ID @User
      & giveID uid
      & buildRoute
  route (ModifyCurrentUser _) =
    baseRoute
      & buildRoute
  route (GetCurrentUserGuilds _) =
    baseRoute // S "guilds"
      & buildRoute
  route (LeaveGuild (getID @Guild -> gid)) =
    baseRoute // S "guilds" // ID @Guild
      & giveID gid
      & buildRoute
  route (CreateDM _) =
    baseRoute // S "channels"
      & buildRoute

  action GetCurrentUser = getWith
  action (GetUser _) = getWith
  action (ModifyCurrentUser o) = patchWith' $ ReqBodyJson o
  action (GetCurrentUserGuilds GetCurrentUserGuildsOptions {before, after, limit}) =
    getWithP
      ( "before" =:? (fromSnowflake <$> before) <> "after" =:? (fromSnowflake <$> after)
          <> "limit" =:? limit
      )
  action (LeaveGuild _) = deleteWith
  action (CreateDM (getID @User -> uid)) = postWith' $ ReqBodyJson (Aeson.object ["recipient_id" Aeson..= uid])

$(makeFieldLabelsNoPrefix ''ModifyUserData)
$(makeFieldLabelsNoPrefix ''GetCurrentUserGuildsOptions)
