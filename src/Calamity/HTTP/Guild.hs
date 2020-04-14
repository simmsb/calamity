-- | Guild endpoints
module Calamity.HTTP.Guild
    ( GuildRequest(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.User
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake

import           Data.Aeson

import           Network.Wreq

data CreateGuildData = CreateGuildData
  { name                        :: ShortText
  , region                      :: ShortText
  , icon                        :: ShortText
  , verificationLevel           :: Integer -- TODO: enums for these
  , defaultMessageNotifications :: Integer
  , explicitContentFilter       :: Integer
  , roles                       :: [Role]
  , channels                    :: [Partial Channel]
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON CreateGuildData

data ModifyGuildData = ModifyGuildData
  { name                        :: Maybe ShortText
  , region                      :: Maybe ShortText
  , icon                        :: Maybe ShortText
  , verificationLevel           :: Maybe Integer -- TODO: enums for these
  , defaultMessageNotifications :: Maybe Integer
  , explicitContentFilter       :: Maybe Integer
  , afkChannelID                :: Maybe (Snowflake Channel)
  , afkTimeout                  :: Maybe Integer
  , ownerID                     :: Maybe (Snowflake User)
  , splash                      :: Maybe ShortText
  , banner                      :: Maybe ShortText
  , systemChannelID             :: Maybe (Snowflake Channel)
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON ModifyGuildData

data GuildRequest a where
  CreateGuild :: CreateGuildData -> GuildRequest Guild
  GetGuild    :: HasSpecificID g Guild => g -> GuildRequest Guild
  ModifyGuild :: HasSpecificID g Guild => g -> ModifyGuildData -> GuildRequest Guild

baseRoute :: Snowflake Guild -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "guilds" // ID @Guild
  & giveID id

instance Request (GuildRequest a) a where
  toRoute (CreateGuild _) = mkRouteBuilder // S "guilds"
    & buildRoute
  toRoute (GetGuild (getID -> gid)) = baseRoute gid
    & buildRoute
  toRoute (ModifyGuild (getID -> gid) _) = baseRoute gid
    & buildRoute

  toAction (CreateGuild o) = postWith' (toJSON o)
  toAction (GetGuild _) = getWith
  toAction (ModifyGuild _ o) = patchWith' (toJSON o)
