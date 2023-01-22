{-# LANGUAGE TemplateHaskell #-}

-- | Discord Guilds
module Calamity.Types.Model.Guild.Guild (
  Guild (..),
  GuildIcon (..),
  GuildSplash (..),
  GuildDiscoverySplash (..),
  GuildBanner (..),
  Partial (PartialGuild),
  UpdatedGuild (..),
) where

import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import Calamity.Internal.SnowflakeMap qualified as SM
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=))
import Calamity.Types.CDNAsset (CDNAsset (..))
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Emoji
import Calamity.Types.Model.Guild.Member
import Calamity.Types.Model.Guild.Role
import Calamity.Types.Model.Presence.Presence
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice.VoiceState
import Calamity.Types.Snowflake
import Calamity.Utils.CDNUrl (assetHashFile, cdnURL)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as LH
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Data.Word
import Network.HTTP.Req ((/:), (/~))
import Optics
import TextShow qualified
import TextShow.TH (deriveTextShow)

data GuildIcon = GuildIcon
  { guildID :: Snowflake Guild
  , hash :: T.Text
  }
  deriving (Show, Eq)

instance CDNAsset GuildIcon where
  assetURL GuildIcon {hash, guildID} =
    cdnURL /: "icons" /~ guildID /: assetHashFile hash

data GuildSplash = GuildSplash
  { guildID :: Snowflake Guild
  , hash :: T.Text
  }
  deriving (Show, Eq)

instance CDNAsset GuildSplash where
  assetURL GuildSplash {hash, guildID} =
    cdnURL /: "splashes" /~ guildID /: assetHashFile hash

data GuildDiscoverySplash = GuildDiscoverySplash
  { guildID :: Snowflake Guild
  , hash :: T.Text
  }
  deriving (Show, Eq)

instance CDNAsset GuildDiscoverySplash where
  assetURL GuildDiscoverySplash {hash, guildID} =
    cdnURL /: "discovery-splashes" /~ guildID /: assetHashFile hash

data GuildBanner = GuildBanner
  { guildID :: Snowflake Guild
  , hash :: T.Text
  }
  deriving (Show, Eq)

instance CDNAsset GuildBanner where
  assetURL GuildBanner {hash, guildID} =
    cdnURL /: "banners" /~ guildID /: assetHashFile hash

data Guild = Guild
  { id :: Snowflake Guild
  , name :: T.Text
  , icon :: Maybe GuildIcon
  , splash :: Maybe GuildSplash
  , discoverySplash :: Maybe GuildSplash
  , banner :: Maybe GuildBanner
  , owner :: Maybe Bool
  , ownerID :: Snowflake User
  , permissions :: Word64
  , afkChannelID :: Maybe (Snowflake GuildChannel)
  , afkTimeout :: Int
  , embedEnabled :: Bool
  , embedChannelID :: Maybe (Snowflake GuildChannel)
  , verificationLevel :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter :: Int
  , roles :: SnowflakeMap Role
  , emojis :: SnowflakeMap Emoji
  , features :: [T.Text]
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
  , preferredLocale :: T.Text
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow Guild
  deriving (HasID Guild) via HasIDField "id" Guild

instance Aeson.FromJSON Guild where
  parseJSON = Aeson.withObject "Guild" $ \v -> do
    id <- v .: "id"

    members' <- do
      members' <- v .: "members"
      pure . SM.fromList . mapMaybe (Aeson.parseMaybe @Aeson.Object @Member (Aeson.parseJSON . Aeson.Object)) $ members'

    channels' <- do
      channels' <- v .: "channels"
      SM.fromList <$> traverse (\c -> Aeson.parseJSON $ Aeson.Object (c <> "guild_id" Aeson..= id)) channels'

    presences' <- do
      presences' <- v .: "presences"
      pure
        . LH.fromList
        . mapMaybe
          ( Aeson.parseMaybe @Aeson.Object @(Snowflake User, Presence)
              ( \m -> do
                  p <- Aeson.parseJSON $ Aeson.Object (m <> "guild_id" Aeson..= id)
                  pure (getID $ p ^. labelOptic @"user", p)
              )
          )
        $ presences'

    icon <- (GuildIcon id <$>) <$> v .:? "icon"
    splash <- (GuildSplash id <$>) <$> v .:? "splash"
    discoverySplash <- (GuildSplash id <$>) <$> v .:? "discovery_splash"
    banner <- (GuildBanner id <$>) <$> v .:? "banner"

    Guild id
      <$> v .: "name"
      <*> pure icon
      <*> pure splash
      <*> pure discoverySplash
      <*> pure banner
      <*> v .:? "owner"
      <*> v .: "owner_id"
      <*> v .:? "permissions" .!= 0
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
      <*> v .:? "unavailable" .!= False
      <*> v .: "member_count"
      <*> v .: "voice_states"
      <*> pure members'
      <*> pure channels'
      <*> pure presences'
      <*> v .: "preferred_locale"

data instance Partial Guild = PartialGuild
  { id :: Snowflake Guild
  , name :: T.Text
  }
  deriving (Eq, Show)
  deriving (HasID Guild) via HasIDField "id" (Partial Guild)
  deriving (Aeson.ToJSON) via CalamityToJSON (Partial Guild)

instance CalamityToJSON' (Partial Guild) where
  toPairs PartialGuild {..} =
    [ "id" .= id
    , "name" .= name
    ]

instance Aeson.FromJSON (Partial Guild) where
  parseJSON = Aeson.withObject "Partial Guild" $ \v ->
    PartialGuild
      <$> v .: "id"
      <*> v .: "name"

data UpdatedGuild = UpdatedGuild
  { id :: Snowflake Guild
  , name :: T.Text
  , icon :: Maybe GuildIcon
  , splash :: Maybe GuildSplash
  , discoverySplash :: Maybe GuildSplash
  , banner :: Maybe GuildBanner
  , owner :: Maybe Bool
  , ownerID :: Snowflake User
  , permissions :: Maybe Word64
  , afkChannelID :: Maybe (Snowflake GuildChannel)
  , afkTimeout :: Int
  , embedEnabled :: Maybe Bool
  , embedChannelID :: Maybe (Snowflake GuildChannel)
  , verificationLevel :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter :: Int
  , roles :: SnowflakeMap Role
  , emojis :: SnowflakeMap Emoji
  , features :: [T.Text]
  , mfaLevel :: Int
  , applicationID :: Maybe (Snowflake User)
  , widgetEnabled :: Maybe Bool
  , widgetChannelID :: Maybe (Snowflake GuildChannel)
  , systemChannelID :: Maybe (Snowflake GuildChannel)
  , preferredLocale :: T.Text
  }
  deriving (Eq, Show)
  deriving (TextShow.TextShow) via TextShow.FromStringShow UpdatedGuild
  deriving (HasID Guild) via HasIDField "id" UpdatedGuild

instance Aeson.FromJSON UpdatedGuild where
  parseJSON = Aeson.withObject "Guild" $ \v -> do
    id <- v .: "id"
    icon <- (GuildIcon id <$>) <$> v .:? "icon"
    splash <- (GuildSplash id <$>) <$> v .:? "splash"
    discoverySplash <- (GuildSplash id <$>) <$> v .:? "discovery_splash"
    banner <- (GuildBanner id <$>) <$> v .:? "banner"

    UpdatedGuild
      <$> v .: "id"
      <*> v .: "name"
      <*> pure icon
      <*> pure splash
      <*> pure discoverySplash
      <*> pure banner
      <*> v .:? "owner"
      <*> v .: "owner_id"
      <*> v .:? "permissions"
      <*> v .:? "afk_channel_id"
      <*> v .: "afk_timeout"
      <*> v .:? "embed_enabled"
      <*> v .:? "embed_channel_id"
      <*> v .: "verification_level"
      <*> v .: "default_message_notifications"
      <*> v .: "explicit_content_filter"
      <*> v .: "roles"
      <*> v .: "emojis"
      <*> v .: "features"
      <*> v .: "mfa_level"
      <*> v .:? "application_id"
      <*> v .:? "widget_enabled"
      <*> v .:? "widget_channel_id"
      <*> v .:? "system_channel_id"
      <*> v .: "preferred_locale"

$(deriveTextShow ''GuildIcon)
$(deriveTextShow 'PartialGuild)

$(makeFieldLabelsNoPrefix ''Guild)
$(makeFieldLabelsNoPrefix ''GuildIcon)
$(makeFieldLabelsNoPrefix 'PartialGuild)
$(makeFieldLabelsNoPrefix ''UpdatedGuild)
