-- | User endpoints
module Calamity.HTTP.User
    ( UserRequest(..)
    , ModifyUserData(..)
    , GetCurrentUserGuildsOptions(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Arrow
import           Control.Lens                   hiding ( (.=) )

import           Data.Aeson
import           Data.Default.Class
import           Data.Maybe
import           Data.Text                      ( Text )

import           GHC.Generics

import           Network.Wreq

import           TextShow

data ModifyUserData = ModifyUserData
  { username :: Maybe Text
    -- | The avatar field should be in discord's image data format: https://discordapp.com/developers/docs/reference#image-data
  , avatar   :: Maybe Text
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON ModifyUserData

data GetCurrentUserGuildsOptions = GetCurrentUserGuildsOptions
  { before :: Maybe (Snowflake Guild)
  , after  :: Maybe (Snowflake Guild)
  , limit  :: Maybe Integer
  }
  deriving ( Show, Generic, Default )

data UserRequest a where
  GetCurrentUser       ::                                UserRequest User
  GetUser              :: HasID User u => u ->           UserRequest User
  ModifyCurrentUser    :: ModifyUserData ->              UserRequest User
  GetCurrentUserGuilds :: GetCurrentUserGuildsOptions -> UserRequest [Partial Guild]
  LeaveGuild           :: HasID Guild g => g ->          UserRequest ()
  CreateDM             :: HasID User u => u ->           UserRequest DMChannel

baseRoute :: RouteBuilder _
baseRoute = mkRouteBuilder // S "users" // S "@me"

instance Request (UserRequest a) a where
  toRoute GetCurrentUser = baseRoute
    & buildRoute
  toRoute (GetUser (getID @User -> uid)) = mkRouteBuilder // S "users" // ID @User
    & giveID uid
    & buildRoute
  toRoute (ModifyCurrentUser _) = baseRoute
    & buildRoute
  toRoute (GetCurrentUserGuilds _) = baseRoute // S "guilds"
    & buildRoute
  toRoute (LeaveGuild (getID @Guild -> gid)) = baseRoute // S "guilds" // ID @Guild
    & giveID gid
    & buildRoute
  toRoute (CreateDM _) = baseRoute // S "channels"
    & buildRoute

  toAction GetCurrentUser = getWith
  toAction (GetUser _) = getWith
  toAction (ModifyCurrentUser o) = patchWith' (toJSON o)
  toAction (GetCurrentUserGuilds GetCurrentUserGuildsOptions { before, after, limit }) = getWithP
    (param "before" .~ maybeToList (showt <$> before) >>> param "after" .~ maybeToList (showt <$> after) >>> param
     "limit" .~ maybeToList (showt <$> limit))
  toAction (LeaveGuild _) = deleteWith
  toAction (CreateDM (getID @User -> uid)) = postWith' (object ["recipient_id" .= uid])
