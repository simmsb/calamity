-- | Discord Guilds
module Calamity.Types.Model.Guild.Guild (
  Guild (..),
  Partial (PartialGuild),
  UpdatedGuild (..),
) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import qualified Calamity.Internal.SnowflakeMap as SM
import Calamity.Internal.Utils
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Emoji
import Calamity.Types.Model.Guild.Member
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.Presence.Presence
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice.VoiceState
import Calamity.Types.Snowflake
import Control.DeepSeq
import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.Types
import Data.Generics.Product.Fields
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as LH
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Word
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Guild' = Guild'
  { id :: Snowflake Guild
  , name :: Text
  , icon :: Maybe Text
  , splash :: Maybe Text
  , owner :: Maybe Bool
  , ownerID :: Snowflake User
  , permissions :: Word64
  , region :: Text
  , afkChannelID :: Maybe (Snowflake GuildChannel)
  , afkTimeout :: Int
  , embedEnabled :: Bool
  , embedChannelID :: Maybe (Snowflake GuildChannel)
  , verificationLevel :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter :: Int
  , roles :: SnowflakeMap Role
  , emojis :: SnowflakeMap Emoji
  , features :: [Text]
  , mfaLevel :: Int
  , applicationID :: Maybe (Snowflake User)
  , widgetEnabled :: Bool
  , widgetChannelID :: Maybe (Snowflake GuildChannel)
  , systemChannelID :: Maybe (Snowflake GuildChannel)
  , -- NOTE: Below are only sent on GuildCreate
    joinedAt :: Maybe (CalamityFromStringShow UTCTime)
  , large :: Bool
  , unavailable :: Bool
  , memberCount :: Int
  , voiceStates :: [VoiceState]
  , members :: SnowflakeMap Member
  , channels :: SnowflakeMap GuildChannel
  , presences :: CalamityFromStringShow (HashMap (Snowflake User) Presence)
  }
  deriving (Generic)
  deriving (TextShow) via TSG.FromGeneric Guild'

data Guild = Guild
  { id :: Snowflake Guild
  , name :: Text
  , icon :: Maybe Text
  , splash :: Maybe Text
  , owner :: Maybe Bool
  , ownerID :: Snowflake User
  , permissions :: Word64
  , region :: Text
  , afkChannelID :: Maybe (Snowflake GuildChannel)
  , afkTimeout :: Int
  , embedEnabled :: Bool
  , embedChannelID :: Maybe (Snowflake GuildChannel)
  , verificationLevel :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter :: Int
  , roles :: SnowflakeMap Role
  , emojis :: SnowflakeMap Emoji
  , features :: [Text]
  , mfaLevel :: Int
  , applicationID :: Maybe (Snowflake User)
  , widgetEnabled :: Bool
  , widgetChannelID :: Maybe (Snowflake GuildChannel)
  , systemChannelID :: Maybe (Snowflake GuildChannel)
  , -- NOTE: Below are only sent on GuildCreate
    joinedAt :: Maybe UTCTime
  , large :: Bool
  , unavailable :: Bool
  , memberCount :: Int
  , voiceStates :: [VoiceState]
  , members :: SnowflakeMap Member
  , channels :: SnowflakeMap GuildChannel
  , presences :: HashMap (Snowflake User) Presence
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via OverriddenVia Guild Guild'
  deriving (HasID Guild) via HasIDField "id" Guild

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \v -> do
    id <- v .: "id"

    -- sadly we have now way of logging members/channels/presences' that failed to parser here
    members' <- do
      members' <- v .: "members"
      pure . SM.fromList . mapMaybe (parseMaybe @Object @Member (\m -> parseJSON $ Object (m <> "guild_id" .= id))) $ members'

    channels' <- do
      channels' <- v .: "channels"
      pure . SM.fromList . mapMaybe (parseMaybe @Object @GuildChannel (\m -> parseJSON $ Object (m <> "guild_id" .= id))) $ channels'

    presences' <- do
      presences' <- v .: "presences"
      pure . LH.fromList
        . mapMaybe
          ( parseMaybe @Object @(Snowflake User, Presence)
              ( \m -> do
                  p <- parseJSON $ Object (m <> "guild_id" .= id)
                  pure (getID $ p ^. field @"user", p)
              )
          )
        $ presences'

    Guild id
      <$> v .: "name"
      <*> v .: "icon"
      <*> v .:? "splash"
      <*> v .:? "owner"
      <*> v .: "owner_id"
      <*> v .:? "permissions" .!= 0
      <*> v .: "region"
      <*> v .:? "afk_channel_id"
      <*> v .: "afk_timeout"
      <*> v .:? "embed_enabled" .!= False
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
  { id :: Snowflake Guild
  , name :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (TextShow) via TSG.FromGeneric (Partial Guild)
  deriving (ToJSON, FromJSON) via CalamityJSON (Partial Guild)
  deriving (HasID Guild) via HasIDField "id" (Partial Guild)

data UpdatedGuild = UpdatedGuild
  { id :: Snowflake Guild
  , name :: Text
  , icon :: Maybe Text
  , splash :: Maybe Text
  , owner :: Maybe Bool
  , ownerID :: Snowflake User
  , permissions :: Maybe Word64
  , region :: Text
  , afkChannelID :: Maybe (Snowflake GuildChannel)
  , afkTimeout :: Int
  , embedEnabled :: Maybe Bool
  , embedChannelID :: Maybe (Snowflake GuildChannel)
  , verificationLevel :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter :: Int
  , roles :: SnowflakeMap Role
  , emojis :: SnowflakeMap Emoji
  , features :: [Text]
  , mfaLevel :: Int
  , applicationID :: Maybe (Snowflake User)
  , widgetEnabled :: Maybe Bool
  , widgetChannelID :: Maybe (Snowflake GuildChannel)
  , systemChannelID :: Maybe (Snowflake GuildChannel)
  }
  deriving (Eq, Show, Generic)
  deriving (TextShow) via TSG.FromGeneric UpdatedGuild
  deriving (FromJSON) via CalamityJSON UpdatedGuild
  deriving (HasID Guild) via HasIDField "id" UpdatedGuild
