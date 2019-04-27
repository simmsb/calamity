-- | General data types
module Calamity.Types.General
    ( Token(..)
    , VoiceState(..)
    , User(..)
    , Channel(..)
    , TextChannel(..)
    , VoiceChannel(..)
    , GuildChannel(..)
    , DMChannel(..)
    , ChannelType(..)
    , SingleDM(..)
    , GroupDM(..)
    , Category(..)
    , Guild(..)
    , UpdatedGuild(..)
    , UnavailableGuild(..)
    , Member(..)
    , Message(..)
    , UpdatedMessage(..)
    , Emoji(..)
    , Role(..)
    , Reaction(..)
    , Presence(..)
    , Embed(..)
    , Attachment(..)
    , formatToken
    , rawToken ) where

import           Calamity.Types.Partial
import           Calamity.Types.Snowflake
import qualified Calamity.Types.SnowflakeMap  as SM
import           Calamity.Types.SnowflakeMap  ( SnowflakeMap(..) )
import           Calamity.Types.UnixTimestamp

import           Control.Monad

import           Data.Aeson
import           Data.Generics.Product.Fields
import           Data.Scientific
import           Data.Time
import           Data.Vector                  ( Vector )

-- Unfortunately all our data models have to go in here since we share a lot of types
data Token
  = BotToken Text
  | UserToken Text
  deriving ( Generic, Show )

formatToken :: Token -> Text
formatToken (BotToken t) = "Bot " <> t
formatToken (UserToken t) = t

rawToken :: Token -> Text
rawToken (BotToken t) = t
rawToken (UserToken t) = t

fuseTup2 :: Monad f => (f a, f b) -> f (a, b)
fuseTup2 (a, b) = do
  a' <- a
  b' <- b
  pure (a', b')

data VoiceState = VoiceState
  { guildID   :: !(Maybe (Snowflake Guild))
  , channelID :: !(Maybe (Snowflake VoiceChannel))
  , userID    :: !(Snowflake User)
  , member    :: !(Maybe Member)
  , sessionID :: !Text
  , deaf      :: !Bool
  , mute      :: !Bool
  , selfDeaf  :: !Bool
  , selfMute  :: !Bool
  , suppress  :: !Bool
  }
  deriving ( Show, Eq, Generic )

instance ToJSON VoiceState where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON VoiceState where
  parseJSON = genericParseJSON jsonOptions

data User = User
  { id            :: !(Snowflake User)
  , username      :: !ShortText
  , discriminator :: !ShortText
  , bot           :: !(Maybe Bool)
  , avatar        :: !(Maybe ShortText)
  , mfaEnabled    :: !(Maybe Bool)
  , verified      :: !(Maybe Bool)
  , email         :: !(Maybe ShortText)
  , flags         :: !(Maybe Word64)
  , premiumType   :: !(Maybe Word64)
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON User where
  parseJSON = genericParseJSON jsonOptions

data instance Partial User = PartialUser
  { id :: Snowflake (Partial User)
  } deriving (Show, Eq, Generic)

instance ToJSON (Partial User) where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON (Partial User) where
  parseJSON = genericParseJSON jsonOptions


data Channel = Channel
  { id                   :: !(Snowflake Channel)
  , type_                :: !ChannelType
  , guildID              :: !(Maybe (Snowflake Guild))
  , position             :: !(Maybe Int)
  , permissionOverwrites :: Maybe (Vector Overwrite)
  , name                 :: !(Maybe ShortText)
  , topic                :: !(Maybe ShortText)
  , nsfw                 :: !(Maybe Bool)
  , lastMessageID        :: !(Maybe (Snowflake Message))
  , bitrate              :: !(Maybe Int)
  , userLimit            :: !(Maybe Int)
  , rateLimitPerUser     :: !(Maybe Int)
  , recipients           :: Maybe (SnowflakeMap User)
  , icon                 :: !(Maybe ShortText)
  , ownerID              :: Maybe (Snowflake User)
  , applicationID        :: Maybe (Snowflake User)
  , parentID             :: Maybe (Snowflake Category)
  , lastPinTimestamp     :: !(Maybe UTCTime)
  } deriving (Show, Eq, Generic)

instance ToJSON Channel where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Channel where
  parseJSON = genericParseJSON jsonOptions

data SingleDM = SingleDM
  { id            :: Snowflake SingleDM
  , lastMessageID :: Maybe (Snowflake Message)
  , recipients    :: SnowflakeMap User
  } deriving (Show, Eq, Generic)

data GroupDM = GroupDM
  { id            :: Snowflake GroupDM
  , ownerID       :: Snowflake User
  , lastMessageID :: Maybe (Snowflake Message)
  , icon          :: Maybe ShortText
  , recipients    :: SnowflakeMap User
  , name          :: ShortText
  } deriving (Show, Eq, Generic)

data DMChannel
  = Single SingleDM
  | Group GroupDM
  deriving (Show, Eq, Generic)

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  deriving (Show, Eq, Generic)

instance {-# OVERLAPS #-} HasID GuildChannel where
  getID = coerceSnowflake . getID

-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hsfield#L182)
data ChannelType
  = GuildTextType
  | DMType
  | GuildVoiceType
  | GroupDMType
  | GuildCategoryType
  deriving (Eq, Generic, Show, Enum)

instance ToJSON ChannelType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ChannelType where
  parseJSON = withScientific "ChannelType"  $ \n ->
    case toBoundedInteger n of
      Just v  -> return $ toEnum v
      Nothing -> fail $ "Invalid ChannelType: " ++ show n

data Category = Category
  { id                   :: Snowflake Category
  , permissionOverwrites :: Vector Overwrite
  , name                 :: ShortText
  , nsfw                 :: Bool
  , position             :: Int
  , guildID              :: Snowflake Guild
  , channels             :: SnowflakeMap GuildChannel
  } deriving (Show, Eq, Generic)

data TextChannel = TextChannel
  { id                   :: Snowflake TextChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: Vector Overwrite
  , name                 :: ShortText
  , topic                :: ShortText
  , nsfw                 :: Bool
  , lastMessageID        :: Maybe (Snowflake Message)
  , rateLimitPerUser     :: Maybe Int
  , parentID             :: Maybe (Snowflake Category)
  } deriving (Show, Eq, Generic)

data VoiceChannel = VoiceChannel
  { id                   :: Snowflake VoiceChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: Vector Overwrite
  , name                 :: ShortText
  , bitrate              :: Int
  , userLimit            :: Int
  } deriving (Show, Eq, Generic)

data Guild = Guild
  { id                          :: !(Snowflake Guild)
  , name                        :: !ShortText
  , icon                        :: !(Maybe ShortText)
  , splash                      :: !(Maybe ShortText)
  , owner                       :: !(Maybe Bool)
  , ownerID                     :: !(Snowflake User)
  , permissions                 :: !Word64
  , region                      :: !ShortText
  , afkChannelID                :: !(Maybe (Snowflake GuildChannel))
  , afkTimeout                  :: !Int
  , embedEnabled                :: !Bool
  , embedChannelID              :: !(Maybe (Snowflake GuildChannel))
  , verificationLevel           :: !Int
  , defaultMessageNotifications :: !Int
  , explicitContentFilter       :: !Int
  , roles                       :: !(SnowflakeMap Role)
  , emojis                      :: !(SnowflakeMap Emoji)
  , features                    :: !(Vector ShortText)
  , mfaLevel                    :: !Int
  , applicationID               :: !(Maybe (Snowflake User))
  , widgetEnabled               :: !Bool
  , widgetChannelID             :: !(Maybe (Snowflake GuildChannel))
  , systemChannelID             :: !(Maybe (Snowflake GuildChannel))
  -- NOTE: Below are only sent on GuildCreate
  , joinedAt                    :: !(Maybe UTCTime)
  , large                       :: !Bool
  , unavailable                 :: !Bool
  , memberCount                 :: !Int
  , voiceStates                 :: !(Vector VoiceState)
  , members                     :: !(SnowflakeMap Member)
  , channels                    :: !(SnowflakeMap Channel)
  , presences                   :: !(Vector Presence)
  } deriving (Eq, Show, Generic)

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

instance FromJSON UpdatedGuild where
  parseJSON = withObject "UpdatedGuild" $ \v -> UpdatedGuild
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "icon"
    <*> v .:? "splash"
    <*> v .:? "owner"
    <*> v .: "owner_id"
    <*> v .:? "permissions"
    <*> v .: "region"
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

-- TODO: eventually use these for lenses
-- buildChannels :: forall a. (FromChannel a, FromRet a ~ Either Text a) => Snowflake Guild -> SnowflakeMap Channel -> SnowflakeMap a
-- buildChannels guildID = SM.mapMaybe (rightToMaybe . fromChannelWithGuildID guildID (Proxy @a))

-- buildCategories :: Snowflake Guild -> SnowflakeMap Channel -> SnowflakeMap Category
-- buildCategories guildID chans = SM.mapMaybe buildCat chans
--       where buildCat chan@(Channel {type_ = GuildCategoryType}) = do
--               let guildChannels -- :: SnowflakeMap GuildChannel
--                     = buildChannels guildID chans
--               rightToMaybe $ fromChannelWithGuildID guildID (Proxy @Category) chan guildChannels
--             buildCat _ = Nothing

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \v -> do
    id <- v.: "id"

    members' <- do
      members' <- v .: "members"
      SM.fromList <$> mapM (\m -> parseJSON $ Object (m <> "guild_id" .= id)) members'

    channels' <- do
      channels' <- v .: "channels"
      SM.fromList <$> mapM (\m -> parseJSON $ Object (m <> "guild_id" .= id)) channels'

    presences' <- do
      presences' <- v .: "presences"
      mapM (\m -> parseJSON $ Object (m <> "guild_id" .= id)) presences'

    Guild
      <$> pure id
      <*> v .: "name"
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


data UnavailableGuild = UnavailableGuild
  { id          :: !(Snowflake Guild)
  , unavailable :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UnavailableGuild where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON UnavailableGuild where
  parseJSON = genericParseJSON jsonOptions


data Member = Member
  { user     :: !User
  , guildID  :: !(Snowflake Guild)
  , nick     :: !(Maybe ShortText)
  , roles    :: !(Vector (Snowflake Role))
  , joinedAt :: !UTCTime
  , deaf     :: !Bool
  , mute     :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Member where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Member where
  parseJSON = genericParseJSON jsonOptions

instance HasID Member where
  getID = coerceSnowflake . getID . (^. field @"user")

-- NOTE: make sure we fill in the guildID field when retrieving from REST
data Message = Message
  { id              :: !(Snowflake Message)
  , channelID       :: !(Snowflake Channel)
  , guildID         :: !(Snowflake Guild)
  , author          :: !User
  , content         :: !ShortText
  , timestamp       :: !UTCTime
  , editedTimestamp :: !(Maybe UTCTime)
  , tts             :: !Bool
  , mentionEveryone :: !Bool
  , mentions        :: !(SnowflakeMap User)
  , mentionRoles    :: !(Vector (Snowflake Role))
  , attachments     :: !(Vector Attachment)
  , embeds          :: !(Vector Embed)
  , reactions       :: !(Vector Reaction)
  , nonce           :: !(Maybe (Snowflake Message))
  , pinned          :: !Bool
  , webhookID       :: !(Maybe (Snowflake ()))
  , type_           :: !MessageType
  } deriving (Eq, Show, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "id"
    <*> v .: "channel_id"
    <*> v .: "guild_id"
    <*> v .: "author"
    <*> v .: "content"
    <*> v .: "timestamp"
    <*> v .:? "edited_timestamp"
    <*> v .: "tts"
    <*> v .: "mention_everyone"
    <*> v .: "mentions"
    <*> v .: "mention_roles"
    <*> v .: "attachments"
    <*> v .: "embeds"
    <*> v .:? "reactions" .!= mempty
    <*> v .:? "nonce"
    <*> v .: "pinned"
    <*> v .:? "webhook_id"
    <*> v .: "type"

data UpdatedMessage = UpdatedMessage
  { id              :: Snowflake UpdatedMessage
  , channelID       :: Snowflake Channel
  , content         :: Maybe ShortText
  , editedTimestamp :: Maybe UTCTime
  , tts             :: Maybe Bool
  , mentionEveryone :: Maybe Bool
  , mentions        :: Maybe (SnowflakeMap User)
  , mentionRoles    :: Maybe (Vector (Snowflake Role))
  , attachments     :: Maybe (Vector Attachment)
  , embeds          :: Maybe (Vector Embed)
  , reactions       :: Maybe (Vector Reaction)
  , pinned          :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance FromJSON UpdatedMessage where
  parseJSON = genericParseJSON jsonOptions


-- Thanks sbrg (https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs#L264)
data MessageType
    = Default
    | RecipientAdd
    | RecipientRemove
    | Call
    | ChannelNameChange
    | ChannelIconChange
    | ChannelPinnedMessage
    | GuildMemberJoin
    deriving (Eq, Show, Enum)

instance ToJSON MessageType where
    toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON MessageType where
    parseJSON = withScientific "MessageType"  $ \n ->
        case toBoundedInteger n of
            Just v  -> return $ toEnum v
            Nothing -> fail $ "Invalid MessageType: " ++ show n


data Embed = Embed
  { title       :: !(Maybe ShortText)
  , type_       :: !(Maybe ShortText)
  , description :: !(Maybe ShortText)
  , url         :: !(Maybe ShortText)
  , timestamp   :: !(Maybe UTCTime)
  , color       :: !(Maybe Word64)
  , footer      :: Maybe EmbedFooter
  , image       :: Maybe EmbedImage
  , thumbnail   :: Maybe EmbedThumbnail
  , video       :: Maybe EmbedVideo
  , provider    :: Maybe EmbedProvider
  , author      :: Maybe EmbedAuthor
  , fields      :: [EmbedField]
  } deriving (Eq, Show, Generic)

instance ToJSON Embed  where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Embed where
  parseJSON = withObject "Embed" $ \v -> Embed
    <$> v .:? "title"
    <*> v .:? "type"
    <*> v .:? "description"
    <*> v .:? "url"
    <*> v .:? "timestamp"
    <*> v .:? "color"
    <*> v .:? "footer"
    <*> v .:? "image"
    <*> v .:? "thumbnail"
    <*> v .:? "video"
    <*> v .:? "provider"
    <*> v .:? "author"
    <*> v .:? "fields" .!= []

data EmbedFooter = EmbedFooter
  { text         :: !ShortText
  , iconUrl      :: !(Maybe ShortText)
  , proxyIconUrl :: !(Maybe ShortText)
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedFooter where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedFooter where
  parseJSON = genericParseJSON jsonOptions

data EmbedImage = EmbedImage
  { url        :: !(Maybe ShortText)
  , proxyUrl   :: !(Maybe ShortText)
  , dimensions :: !(Maybe (Word64, Word64)) -- doesn't make sense to have only one of the width or height
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedImage where
  toEncoding EmbedImage{url, proxyUrl, dimensions = Just (width, height)} =
    pairs ("url" .= url <> "proxy_url" .= proxyUrl <>
           "width" .= width <> "height" .= height)

  toEncoding EmbedImage{url, proxyUrl} =
    pairs ("url" .= url <> "proxy_url" .= proxyUrl)

instance FromJSON EmbedImage where
  parseJSON = withObject "EmbedImage" $ \v -> do
    width  <- v .:? "width"
    height <- v .:? "height"

    EmbedImage
      <$> v .:? "url"
      <*> v .:? "proxy_url"
      <*> pure (fuseTup2 (width, height))

data EmbedThumbnail = EmbedThumbnail
  { url        :: !(Maybe ShortText)
  , proxyUrl   :: !(Maybe ShortText)
  , dimensions :: !(Maybe (Word64, Word64)) -- doesn't make sense to have only one of the width or height
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedThumbnail where
  toEncoding EmbedThumbnail{url, proxyUrl, dimensions = Just (width, height)} =
    pairs ("url" .= url <> "proxy_url" .= proxyUrl <>
           "width" .= width <> "height" .= height)

  toEncoding EmbedThumbnail{url, proxyUrl} =
    pairs ("url" .= url <> "proxy_url" .= proxyUrl)

instance FromJSON EmbedThumbnail where
  parseJSON = withObject "EmbedThumbnail" $ \v -> do
    width  <- v .:? "width"
    height <- v .:? "height"

    EmbedThumbnail
      <$> v .:? "url"
      <*> v .:? "proxy_url"
      <*> pure (fuseTup2 (width, height))

data EmbedVideo = EmbedVideo
  { url        :: !(Maybe ShortText)
  , dimensions :: !(Maybe (Word64, Word64)) -- doesn't make sense to have only one of the width or height
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedVideo where
  toEncoding EmbedVideo{url, dimensions = Just (width, height)} =
    pairs ("url" .= url <> "width" .= width <> "height" .= height)

  toEncoding EmbedVideo{url} =
    pairs ("url" .= url)

instance FromJSON EmbedVideo where
  parseJSON = withObject "EmbedVideo" $ \v -> do
    width  <- v .:? "width"
    height <- v .:? "height"

    EmbedVideo
      <$> v .:? "url"
      <*> pure (fuseTup2 (width, height))

data EmbedProvider = EmbedProvider
  { name :: !(Maybe ShortText)
  , url  :: !(Maybe ShortText)
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedProvider where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedProvider where
  parseJSON = genericParseJSON jsonOptions

data EmbedAuthor = EmbedAuthor
  { name         :: !(Maybe ShortText)
  , url          :: !(Maybe ShortText)
  , iconUrl      :: !(Maybe ShortText)
  , proxyIconURL :: !(Maybe ShortText)
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedAuthor where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedAuthor where
  parseJSON = genericParseJSON jsonOptions

data EmbedField = EmbedField
  { name   :: !ShortText
  , value  :: !ShortText
  , inline :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedField where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedField where
  parseJSON = withObject "EmbedField" $ \v -> EmbedField
    <$> v .: "name"
    <*> v .: "value"
    <*> v .:? "inline" .!= False

data Attachment = Attachment
  { id         :: !(Snowflake Attachment)
  , filename   :: !ShortText
  , size       :: !Word64
  , url        :: !ShortText
  , proxyUrl   :: !ShortText
  , dimensions :: !(Maybe (Word64, Word64))
  } deriving (Eq, Show, Generic)

instance ToJSON Attachment where
  toEncoding Attachment{id, filename, size, url, proxyUrl,
                        dimensions = Just (width, height)} =
    pairs ("id" .= id <> "filename" .= filename <> "size" .= size <>
           "url" .= url <> "proxy_url" .= proxyUrl <>
           "width" .= width <> "height" .= height)

  toEncoding Attachment{id, filename, size, url, proxyUrl} =
    pairs ("id" .= id <> "filename" .= filename <> "size" .= size <>
           "url" .= url <> "proxy_url" .= proxyUrl)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v -> do
    width  <- v .:? "width"
    height <- v .:? "height"

    Attachment
      <$> v .: "id"
      <*> v .: "filename"
      <*> v .: "size"
      <*> v .: "url"
      <*> v .: "proxy_url"
      <*> pure (fuseTup2 (width, height))

data Emoji = Emoji
  { id            :: !(Snowflake Emoji)
  , name          :: !ShortText
  , roles         :: !(Vector (Snowflake Role))
  , user          :: !(Maybe User)
  , requireColons :: !Bool
  , managed       :: !Bool
  , animated      :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Emoji where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \v -> Emoji
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "roles"
    <*> v .:? "user"
    <*> v .:? "requireColons" .!= False
    <*> v .:? "managed"       .!= False
    <*> v .:? "animated"      .!= False

data instance Partial Emoji = PartialEmoji
  { id   :: Snowflake Emoji
  , name :: ShortText
  }
  deriving ( Eq, Show, Generic )

instance ToJSON (Partial Emoji) where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON (Partial Emoji) where
  parseJSON = genericParseJSON jsonOptions

data Role = Role
  { id          :: !(Snowflake Role)
  , name        :: !ShortText
  , color       :: !Word64
  , hoist       :: !Bool
  , position    :: !Int
  , permissions :: !Word64
  , managed     :: !Bool
  , mentionable :: !Bool
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Role where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Role where
  parseJSON = genericParseJSON jsonOptions

data Overwrite = Overwrite
  { id    :: !(Snowflake Overwrite)
  , type_ :: !ShortText
  , allow :: !Word64
  , deny  :: !Word64
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Overwrite where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Overwrite where
  parseJSON = genericParseJSON jsonOptions

data Reaction = Reaction
  { userID    :: !(Snowflake User)
  , channelID :: !(Snowflake Channel)
  , messageID :: !(Snowflake Message)
  , guildID   :: !(Maybe (Snowflake Guild))
  , emoji     :: !(Either (Partial Emoji) ShortText)
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Reaction where
  toEncoding Reaction { userID, channelID, messageID, guildID, emoji } = pairs
    ("user_id" .= userID <> "channel_id" .= channelID <> "message_id" .= messageID <> "guild_id" .= guildID
     <> case emoji of
       Left emoji  -> "emoji" .= emoji
       Right emoji -> "emoji" .= (("name" .= emoji) :: Object))

instance FromJSON Reaction where
  parseJSON = withObject "Reaction" $ \v -> do
    emoji <- v .: "emoji"

    emojiID :: Maybe (Snowflake Emoji) <- emoji .:? "id"
    emojiName :: ShortText <- emoji .: "name"

    let emoji =
          case emojiID of
            Just id ->
              Left $ PartialEmoji id emojiName
            Nothing ->
              Right emojiName

    Reaction
      <$> v .: "user_id"
      <*> v .: "channel_id"
      <*> v .: "message_id"
      <*> v .:? "guild_id"
      <*> pure emoji


data StatusType
  = Idle
  | DND
  | Online
  | Offline
  deriving (Eq, Show, Enum, Generic)

instance ToJSON StatusType

instance FromJSON StatusType where
  parseJSON = withText "StatusType" $ \case
    "idle"    -> pure Idle
    "dnd"     -> pure DND
    "online"  -> pure Online
    "offline" -> pure Offline
    _         -> fail "Unknown status type"


data Presence = Presence
  { user         :: !(Partial User)
  , roles        :: Maybe (Vector (Snowflake Role))
  , game         :: Maybe Activity
  , guildID      :: !(Snowflake Guild)
  , status       :: !(Maybe StatusType)
  , activities   :: Maybe (Vector Activity)
  , clientStatus :: Maybe ClientStatus
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Presence where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Presence where
  parseJSON = genericParseJSON jsonOptions

data ActivityType
  = Game
  | Streaming
  | Listening
  deriving ( Eq, Generic, Show, Enum )

instance ToJSON ActivityType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ActivityType where
  parseJSON = withScientific "ActivityType" $ \n -> case toBoundedInteger n of
    Just v  -> return $ toEnum v
    Nothing -> fail $ "Invalid ActivityType: " ++ show n

data Activity = Activity
  { name          :: !ShortText
  , type_         :: !ActivityType
  , url           :: !(Maybe ShortText)
  , timestamps    :: !(Maybe ActivityTimestamps)
  , applicationID :: !(Maybe (Snowflake ()))
  , details       :: !(Maybe ShortText)
  , state         :: !(Maybe ShortText)
  , party         :: Maybe ActivityParty
  , assets        :: Maybe ActivityAssets
  , secrets       :: Maybe ActivitySecrets
  , instance_     :: !Bool
  , flags         :: !Word64
  }
  deriving ( Eq, Show, Generic )

instance ToJSON Activity where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \v -> Activity
    <$> v .: "name"
    <*> v .: "type"
    <*> v .:? "url"
    <*> v .:? "timestamps"
    <*> v .:? "applicationID"
    <*> v .:? "details"
    <*> v .:? "state"
    <*> v .:? "party"
    <*> v .:? "assets"
    <*> v .:? "secrets"
    <*> v .:? "instance_" .!= False
    <*> v .:? "flags"     .!= 0

data ActivityTimestamps = ActivityTimestamps
  { start :: Maybe UnixTimestamp
  , end   :: Maybe UnixTimestamp
  } deriving (Eq, Show, Generic)

instance ToJSON ActivityTimestamps where
  toEncoding ActivityTimestamps {start, end} =
    pairs ("start" .= (unixToMilliseconds <$> start)
           <> "end" .= (unixToMilliseconds <$> end))

instance FromJSON ActivityTimestamps where
  parseJSON = withObject "ActivityTimestamps" $ \v -> do
    start <- millisecondsToUnix <<$>> v .:? "start"
    end   <- millisecondsToUnix <<$>> v .:? "end"

    pure $ ActivityTimestamps start end


data ActivityParty = ActivityParty
  { id   :: Maybe ShortText
  , size :: Maybe (Int, Int)
  }
  deriving ( Eq, Show, Generic )

instance ToJSON ActivityParty where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivityParty where
  parseJSON = genericParseJSON jsonOptions

data ActivityAssets = ActivityAssets
  { largeImage :: !(Maybe ShortText)
  , largeText  :: !(Maybe ShortText)
  , smallImage :: !(Maybe ShortText)
  , smallText  :: !(Maybe ShortText)
  }
  deriving ( Eq, Show, Generic )

instance ToJSON ActivityAssets where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivityAssets where
  parseJSON = genericParseJSON jsonOptions

data ActivitySecrets = ActivitySecrets
  { join     :: !(Maybe ShortText)
  , spectate :: !(Maybe ShortText)
  , match    :: !(Maybe ShortText)
  }
  deriving ( Eq, Show, Generic )

instance ToJSON ActivitySecrets where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivitySecrets where
  parseJSON = genericParseJSON jsonOptions

data ClientStatus = ClientStatus
  { desktop :: !(Maybe ShortText)
  , mobile  :: !(Maybe ShortText)
  , web     :: !(Maybe ShortText)
  }
  deriving ( Eq, Show, Generic )

instance ToJSON ClientStatus where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ClientStatus where
  parseJSON = genericParseJSON jsonOptions
