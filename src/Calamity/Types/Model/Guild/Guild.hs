-- | Discord Guilds
module Calamity.Types.Model.Guild.Guild
    ( Guild(..)
    , Partial(PartialGuild)
    , UpdatedGuild(..) ) where

import           Calamity.Internal.AesonThings
import qualified Calamity.Internal.SnowflakeMap         as SM
import           Calamity.Internal.SnowflakeMap         ( SnowflakeMap )
import           Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Guild
import {-# SOURCE #-} Calamity.Types.Model.Guild.Emoji
import {-# SOURCE #-} Calamity.Types.Model.Guild.Member
import           Calamity.Types.Model.Guild.Role
import           Calamity.Types.Model.Presence.Presence
import {-# SOURCE #-} Calamity.Types.Model.User
import           Calamity.Types.Model.Voice.VoiceState
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Generics.Product.Fields
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                      as LH
import           Data.Time
import           Data.Vector                            ( Vector )

data Guild = Guild
  { id                          :: Snowflake Guild
  , name                        :: ShortText
  , icon                        :: Maybe ShortText
  , splash                      :: Maybe ShortText
  , owner                       :: Maybe Bool
  , ownerID                     :: Snowflake User
  , permissions                 :: Word64
  , region                      :: ShortText
  , afkChannelID                :: Maybe (Snowflake GuildChannel)
  , afkTimeout                  :: Int
  , embedEnabled                :: Bool
  , embedChannelID              :: Maybe (Snowflake GuildChannel)
  , verificationLevel           :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter       :: Int
  , roles                       :: SnowflakeMap Role
  , emojis                      :: SnowflakeMap Emoji
  , features                    :: Vector ShortText
  , mfaLevel                    :: Int
  , applicationID               :: Maybe (Snowflake User)
  , widgetEnabled               :: Bool
  , widgetChannelID             :: Maybe (Snowflake GuildChannel)
  , systemChannelID             :: Maybe (Snowflake GuildChannel)
    -- NOTE: Below are only sent on GuildCreate
  , joinedAt                    :: Maybe UTCTime
  , large                       :: Bool
  , unavailable                 :: Bool
  , memberCount                 :: Int
  , voiceStates                 :: Vector VoiceState
  , members                     :: SnowflakeMap Member
  , channels                    :: SnowflakeMap GuildChannel
  , presences                   :: HashMap (Snowflake User) Presence
  }
  deriving ( Eq, Show, Generic )
  deriving ( HasID ) via HasIDField Guild

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \v -> do
    id <- v .: "id"

    -- TODO: clean this up

    members' <- do
      members' <- v .: "members"
      SM.fromList <$> traverse (\m -> parseJSON $ Object (m <> "guild_id" .= id)) members'

    channels' <- do
      channels' <- v .: "channels"
      SM.fromList <$> traverse (\m -> parseJSON $ Object (m <> "guild_id" .= id)) channels'

    presences' <- do
      presences' <- v .: "presences"
      LH.fromList <$> traverse (\m -> do
                                  p <- parseJSON $ Object (m <> "guild_id" .= id)
                                  pure (coerceSnowflake . getID $ p ^. field @"user", p)) presences'

    Guild id
      <$> v .: "name"
      <*> v .: "icon"
      <*> v .:? "splash"
      <*> v .:? "owner"
      <*> v .: "owner_id"
      <*> v .:? "permissions"    .!= 0
      <*> v .: "region"
      <*> v .:? "afk_channel_id"
      <*> v .: "afk_timeout"
      <*> v .:? "embed_enabled"  .!= False
      <*> v .:? "embed_channel_id"
      <*> v .: "verification_level"
      <*> v .: "default_message_notifications"
      <*> v .: "explicit_content_filter"
      <*> v .: "roles"
      <*> v .: "emojis"
      <*> v .: "features"
      <*> v .: "mfa_level"
      <*> v .:? "application_id"
      <*> v .:? "widget_enabled" .!= False
      <*> v .:? "widget_channel_id"
      <*> v .:? "system_channel_id"
      <*> v .:? "joined_at"
      <*> v .: "large"
      <*> v .: "unavailable"
      <*> v .: "member_count"
      <*> v .: "voice_states"
      <*> pure members'
      <*> pure channels'
      <*> pure presences'

data instance Partial Guild = PartialGuild
  { id   :: !(Snowflake Guild)
  , name :: !ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial Guild)
  deriving ( HasID ) via HasIDFieldAlt (Partial Guild) Guild

data UpdatedGuild = UpdatedGuild
  { id                          :: Snowflake Guild
  , name                        :: ShortText
  , icon                        :: Maybe ShortText
  , splash                      :: Maybe ShortText
  , owner                       :: Maybe Bool
  , ownerID                     :: Snowflake User
  , permissions                 :: Maybe Word64
  , region                      :: ShortText
  , afkChannelID                :: Maybe (Snowflake GuildChannel)
  , afkTimeout                  :: Int
  , embedEnabled                :: Maybe Bool
  , embedChannelID              :: Maybe (Snowflake GuildChannel)
  , verificationLevel           :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter       :: Int
  , roles                       :: SnowflakeMap Role
  , emojis                      :: SnowflakeMap Emoji
  , features                    :: Vector ShortText
  , mfaLevel                    :: Int
  , applicationID               :: Maybe (Snowflake User)
  , widgetEnabled               :: Maybe Bool
  , widgetChannelID             :: Maybe (Snowflake GuildChannel)
  , systemChannelID             :: Maybe (Snowflake GuildChannel)
  }
  deriving ( Eq, Show, Generic )
  deriving ( FromJSON ) via CalamityJSON UpdatedGuild
  deriving ( HasID ) via HasIDFieldAlt UpdatedGuild Guild
