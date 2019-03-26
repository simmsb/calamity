-- | General data types

module YAHDL.Types.General
  ( Token(..)
  , VoiceState(..)
  , User(..)
  , FromChannel(..)
  , Channel(..)
  , TextChannel(..)
  , VoiceChannel(..)
  , GuildChannel(..)
  , DMChannel(..)
  , SingleDM(..)
  , GroupDM(..)
  , Category(..)
  , Guild(..)
  , UnavailableGuild(..)
  , Member(..)
  , Message(..)
  , Emoji(..)
  , Role(..)
  , Reaction(..)
  , Presence(..)
  , Embed(..)
  , Attachment(..)
  , formatToken
  , rawToken
  )
where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Generics.Product.Fields
import           Data.Scientific
import           Data.Typeable

import           YAHDL.Types.ISO8601
import           YAHDL.Types.UnixTimestamp
import           YAHDL.Types.Snowflake
import           YAHDL.Types.SnowflakeMap       ( SnowflakeMap(..) )
import qualified YAHDL.Types.SnowflakeMap      as SH

data Token
  = BotToken Text
  | UserToken Text
  deriving (Generic, Show)

formatToken :: Token -> Text
formatToken (BotToken  t) = "Bot " <> t
formatToken (UserToken t) = t

rawToken :: Token -> Text
rawToken (BotToken  t) = t
rawToken (UserToken t) = t

fuseTup2 :: Monad f => (f a, f b) -> f (a, b)
fuseTup2 (a, b) = do
  a' <- a
  b' <- b
  pure (a', b')

data VoiceState = VoiceState
  { guildID   :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake VoiceChannel)
  , userID    :: Snowflake User
  , member    :: Maybe Member
  , sessionID :: Text
  , deaf      :: Bool
  , mute      :: Bool
  , selfDeaf  :: Bool
  , selfMute  :: Bool
  , suppress  :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON VoiceState where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON VoiceState where
  parseJSON = genericParseJSON jsonOptions

data User = User
  { id            :: Snowflake User
  , username      :: Text
  , discriminator :: Text
  , bot           :: Maybe Bool
  , avatar        :: Maybe Text
  , mfaEnabled    :: Maybe Bool
  , verified      :: Maybe Bool
  , email         :: Maybe Text
  , flags         :: Maybe Word64
  , premiumType   :: Maybe Word64
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON User where
  parseJSON = genericParseJSON jsonOptions

-- TODO: scrap fromchannel stuff, all raw events get generic channel, we'll
-- transform into the correct channel sometime later when we meet the cache
-- TODO: make all specific type channels nicer (ie: categories have nested channels, etc)

data Channel = Channel
  { id                   :: Snowflake Channel
  , type_                :: ChannelType
  , guildID              :: Maybe (Snowflake Guild)
  , position             :: Maybe Int
  , permissionOverwrites :: Maybe [Overwrite]
  , name                 :: Maybe Text
  , topic                :: Maybe Text
  , nsfw                 :: Maybe Bool
  , lastMessageID        :: Maybe (Snowflake Message)
  , bitrate              :: Maybe Int
  , userLimit            :: Maybe Int
  , rateLimitPerUser     :: Maybe Int
  , recipients           :: Maybe (SnowflakeMap User)
  , icon                 :: Maybe Text
  , ownerID              :: Maybe (Snowflake User)
  , applicationID        :: Maybe (Snowflake User)
  , parentID             :: Maybe (Snowflake Category)
  , lastPinTimestamp     :: Maybe ISO8601Timestamp
  } deriving (Show, Eq, Generic)

instance ToJSON Channel where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Channel where
  parseJSON = genericParseJSON jsonOptions

defChannel :: Snowflake a -> ChannelType -> Channel
defChannel s t = Channel
                  (coerceSnowflake s)
                  t
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing


class FromChannel a where
  type FromRet a :: *
  type FromRet a = Either Text a

  -- | Convert from a channel into a more specific channel type
  fromChannel :: Proxy a -> Channel -> FromRet a

  -- | Provide a guild ID to a channel when converting to a more specific type
  fromChannelWithGuildID :: Snowflake Guild -> Proxy a -> Channel -> FromRet a
  fromChannelWithGuildID guildID p channel = channel
    & field @"guildID" ?~ guildID
    & (fromChannel p)

  -- | Convert from a specific channel type back to the generic channel type
  toChannel :: a -> Channel

ensureChannelType :: ChannelType -> ChannelType -> Either Text ()
ensureChannelType a b | a == b = Right ()
ensureChannelType a b = Left $ "Channel type "+||a||+" does not match expected "+||b||+""

ensureField :: Text -> Maybe a -> Either Text a
ensureField name = maybeToRight ("Missing field: " <> name)

data SingleDM = SingleDM
  { id            :: Snowflake SingleDM
  , lastMessageID :: Maybe (Snowflake Message)
  , recipients    :: SnowflakeMap User
  } deriving (Show, Eq, Generic)

instance FromChannel SingleDM where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") DMType
    recipients <- ensureField "recipients" $ c ^. field @"recipients"
    pure $ SingleDM (coerceSnowflake $ c ^. field @"id") (c ^. field @"lastMessageID") recipients

  toChannel SingleDM {id, lastMessageID, recipients} = defChannel id DMType
    & field @"lastMessageID" .~ lastMessageID
    & field @"recipients"    ?~ recipients

data GroupDM = GroupDM
  { id            :: Snowflake GroupDM
  , ownerID       :: Snowflake User
  , lastMessageID :: Maybe (Snowflake Message)
  , icon          :: Maybe Text
  , recipients    :: SnowflakeMap User
  , name          :: Text
  } deriving (Show, Eq, Generic)

instance FromChannel GroupDM where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GroupDMType
    owner      <- ensureField "ownerID"    $ c ^. field @"ownerID"
    recipients <- ensureField "recipients" $ c ^. field @"recipients"
    name       <- ensureField "name"       $ c ^. field @"name"
    pure $ GroupDM (coerceSnowflake $ c ^. field @"id") owner (c ^. field @"lastMessageID") (c ^. field @"icon") recipients name

  toChannel GroupDM {id, ownerID, lastMessageID, icon, recipients, name} = defChannel id GroupDMType
    & field @"ownerID"       ?~ ownerID
    & field @"lastMessageID" .~ lastMessageID
    & field @"icon"          .~ icon
    & field @"recipients"    ?~ recipients
    & field @"name"          ?~ name

data DMChannel
  = Single SingleDM
  | Group GroupDM
  deriving (Show, Eq, Generic)

instance FromChannel DMChannel where
  fromChannel _ c@Channel {type_} = case type_ of
    DMType      -> Single <$> fromChannel (Proxy @SingleDM) c
    GroupDMType -> Group  <$> fromChannel (Proxy @GroupDM) c
    _           -> Left "Channel was not one of DMType or GroupDMType"

  toChannel (Single dm) = toChannel dm
  toChannel (Group  dm) = toChannel dm

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  deriving (Show, Eq, Generic)

instance FromChannel GuildChannel where
  fromChannel _ c@Channel {type_} = case type_ of
    GuildTextType     -> GuildTextChannel  <$> fromChannel (Proxy @TextChannel)  c
    GuildVoiceType    -> GuildVoiceChannel <$> fromChannel (Proxy @VoiceChannel) c
    GuildCategoryType -> GuildCategory     <$> fromChannel (Proxy @Category)     c
    _                 -> Left "Channel was not one of GuildTextType, GuildVoiceType, or GuildCategoryType"

  toChannel (GuildTextChannel  c) = toChannel c
  toChannel (GuildVoiceChannel c) = toChannel c
  toChannel (GuildCategory     c) = toChannel c

instance {-# OVERLAPS #-} HasID GuildChannel where
  getID = coerceSnowflake . getID . toChannel

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
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , nsfw                 :: Bool
  , position             :: Int
  , guildID              :: Snowflake Guild
  } deriving (Show, Eq, Generic)

instance FromChannel Category where
  -- type FromRet Category = Either Text Category

  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GuildCategoryType
    permissionOverwrites <- ensureField "permissionOverwrites" $ c ^. field @"permissionOverwrites"
    name                 <- ensureField "name"                 $ c ^. field @"name"
    let nsfw             = fromMaybe False                     $ c ^. field @"nsfw"
    position             <- ensureField "position"             $ c ^. field @"position"
    guildID              <- ensureField "guildID"              $ c ^. field @"guildID"
    pure $ Category (coerceSnowflake $ c ^. field @"id") permissionOverwrites name nsfw position guildID

  toChannel Category {id, permissionOverwrites, name, nsfw, position, guildID} = defChannel id GuildCategoryType
    & field @"permissionOverwrites" ?~ permissionOverwrites
    & field @"name"                 ?~ name
    & field @"nsfw"                 ?~ nsfw
    & field @"position"             ?~ position
    & field @"guildID"              ?~ guildID

data TextChannel = TextChannel
  { id                   :: Snowflake TextChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , topic                :: Maybe Text
  , nsfw                 :: Bool
  , lastMessageID        :: Maybe (Snowflake Message)
  , rateLimitPerUser     :: Maybe Int
  , parentID             :: Maybe (Snowflake Category)
  } deriving (Show, Eq, Generic)

instance FromChannel TextChannel where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GuildTextType
    let id               =  coerceSnowflake $ c ^. field @"id"
    guildID              <- ensureField "guildID"              $ c ^. field @"guildID"
    position             <- ensureField "position"             $ c ^. field @"position"
    permissionOverwrites <- ensureField "permissionOverwrites" $ c ^. field @"permissionOverwrites"
    name                 <- ensureField "name"                 $ c ^. field @"name"
    let topic            =  c ^. field @"topic"
    let nsfw             =  fromMaybe False                    $ c ^. field @"nsfw"
    pure $ TextChannel id guildID position
      permissionOverwrites name topic nsfw
      (c ^. field @"lastMessageID")
      (c ^. field @"rateLimitPerUser")
      (c ^. field @"parentID")

  toChannel TextChannel {id, guildID, position
                        , permissionOverwrites
                        , name, topic, nsfw
                        , lastMessageID
                        , rateLimitPerUser
                        , parentID
                        } = defChannel id GuildTextType
    & field @"guildID"              ?~ guildID
    & field @"position"             ?~ position
    & field @"permissionOverwrites" ?~ permissionOverwrites
    & field @"name"                 ?~ name
    & field @"topic"                .~ topic
    & field @"nsfw"                 ?~ nsfw
    & field @"lastMessageID"        .~ lastMessageID
    & field @"rateLimitPerUser"     .~ rateLimitPerUser
    & field @"parentID"             .~ parentID

data VoiceChannel = VoiceChannel
  { id                   :: Snowflake VoiceChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , bitrate              :: Int
  , userLimit            :: Int
  } deriving (Show, Eq, Generic)

instance FromChannel VoiceChannel where
  fromChannel _ c = do
    ensureChannelType (c ^. field @"type_") GuildVoiceType
    guildID              <- ensureField "guildID"              $ c ^. field @"guildID"
    position             <- ensureField "position"             $ c ^. field @"position"
    permissionOverwrites <- ensureField "permissionOverwrites" $ c ^. field @"permissionOverwrites"
    name                 <- ensureField "name"                 $ c ^. field @"name"
    bitrate              <- ensureField "bitrate"              $ c ^. field @"bitrate"
    userLimit            <- ensureField "userLimit"            $ c ^. field @"userLimit"
    pure $ VoiceChannel (coerceSnowflake $ c ^. field @"id") guildID position permissionOverwrites name bitrate userLimit

  toChannel VoiceChannel {id, guildID, position, permissionOverwrites, name, bitrate, userLimit} = defChannel id GuildVoiceType
    & field @"guildID"               ?~ guildID
    & field @"position"              ?~ position
    & field @"permissionOverwrites"  ?~ permissionOverwrites
    & field @"name"                  ?~ name
    & field @"bitrate"               ?~ bitrate
    & field @"userLimit"             ?~ userLimit


-- TODO: raw guild, complete guild
data Guild = Guild
  { id                          :: Snowflake Guild
  , name                        :: Text
  , icon                        :: Maybe Text
  , splash                      :: Maybe Text
  , owner                       :: Maybe Bool
  , ownerID                     :: Snowflake User
  , permissions                 :: Word64
  , region                      :: Text
  , afkChannelID                :: Maybe (Snowflake GuildChannel)
  , afkTimeout                  :: Int
  , embedEnabled                :: Bool
  , embedChannelID              :: Maybe (Snowflake GuildChannel)
  , verificationLevel           :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter       :: Int
  , roles                       :: [Role]
  , emojis                      :: [Emoji]
  , features                    :: [Text]
  , mfaLevel                    :: Int
  , applicationID               :: Maybe (Snowflake User)
  , widgetEnabled               :: Bool
  , widgetChannelID             :: Maybe (Snowflake GuildChannel)
  , systemChannelID             :: Maybe (Snowflake GuildChannel)
  , joinedAt                    :: Maybe ISO8601Timestamp
  , large                       :: Bool
  , unavailable                 :: Bool
  , memberCount                 :: Int
  , voiceStates                 :: [VoiceState]
  , members                     :: SnowflakeMap Member
  , channels                    :: SnowflakeMap Channel
  , presences                   :: [Presence]
  } deriving (Eq, Show, Generic)

instance ToJSON Guild where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Guild where
  parseJSON = withObject "guild" $ \v -> Guild
    <$> v .: "id"
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
    <*> v .: "members"
    <*> v .: "channels"
    <*> v .: "presences"


data UnavailableGuild = UnavailableGuild
  { id          :: Snowflake Guild
  , unavailable :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UnavailableGuild where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON UnavailableGuild where
  parseJSON = genericParseJSON jsonOptions


data Member = Member
  { user     :: User
  , nick     :: Maybe Text
  , roles    :: [Snowflake Role]
  , joinedAt :: ISO8601Timestamp
  , deaf     :: Bool
  , mute     :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Member where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Member where
  parseJSON = genericParseJSON jsonOptions

instance HasID Member where
  getID = coerceSnowflake . getID . (^. field @"user")

data Message = Message
  { id              :: Snowflake Message
  , channelID       :: Snowflake Channel
  , guildID         :: Maybe (Snowflake Guild)
  , author          :: User
  , content         :: Text
  , timestamp       :: ISO8601Timestamp
  , editedTimestamp :: Maybe ISO8601Timestamp
  , tts             :: Bool
  , mentionEveryone :: Bool
  , mentions        :: SnowflakeMap User
  , mentionRoles    :: [Snowflake Role]
  , attachments     :: [Attachment] -- TODO: snowflakemap these
  , embeds          :: [Embed]
  , reactions       :: [Reaction]
  , nonce           :: Maybe (Snowflake Message)
  , pinned          :: Bool
  , webhookID       :: Maybe (Snowflake ())
  , type_           :: MessageType
  } deriving (Eq, Show, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "id"
    <*> v .: "channel_id"
    <*> v .:? "guild_id"
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
    <*> v .:? "reactions" .!= []
    <*> v .:? "nonce"
    <*> v .: "pinned"
    <*> v .:? "webhook_id"
    <*> v .: "type"

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
  { title       :: Maybe Text
  , type_       :: Maybe Text
  , description :: Maybe Text
  , url         :: Maybe Text
  , timestamp   :: Maybe ISO8601Timestamp
  , color       :: Maybe Word64
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
  { text         :: Text
  , iconUrl      :: Maybe Text
  , proxyIconUrl :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedFooter where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedFooter where
  parseJSON = genericParseJSON jsonOptions

data EmbedImage = EmbedImage
  { url        :: Maybe Text
  , proxyUrl   :: Maybe Text
  , dimensions :: Maybe (Word64, Word64) -- doesn't make sense to have only one of the width or height
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
  { url        :: Maybe Text
  , proxyUrl   :: Maybe Text
  , dimensions :: Maybe (Word64, Word64) -- doesn't make sense to have only one of the width or height
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
  { url        :: Maybe Text
  , dimensions :: Maybe (Word64, Word64) -- doesn't make sense to have only one of the width or height
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
  { name :: Maybe Text
  , url  :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedProvider where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedProvider where
  parseJSON = genericParseJSON jsonOptions

data EmbedAuthor = EmbedAuthor
  { name         :: Maybe Text
  , url          :: Maybe Text
  , iconUrl      :: Maybe Text
  , proxyIconURL :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedAuthor where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedAuthor where
  parseJSON = genericParseJSON jsonOptions

data EmbedField = EmbedField
  { name   :: Text
  , value  :: Text
  , inline :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedField where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EmbedField where
  parseJSON = withObject "EmbedField" $ \v -> EmbedField
    <$> v .: "name"
    <*> v .: "value"
    <*> v .:? "inline" .!= False

data Attachment = Attachment
  { id         :: Snowflake Attachment
  , filename   :: Text
  , size       :: Word64
  , url        :: Text
  , proxyUrl   :: Text
  , dimensions :: Maybe (Word64, Word64)
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
  { id            :: Snowflake Emoji
  , name          :: Text
  , roles         :: [Snowflake Role]
  , user          :: Maybe User
  , requireColons :: Bool
  , managed       :: Bool
  , animated      :: Bool
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

data Role = Role
  { id          :: Snowflake Role
  , name        :: Text
  , color       :: Word64
  , hoist       :: Bool
  , position    :: Int
  , permissions :: Word64
  , managed     :: Bool
  , mentionable :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Role where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Role where
  parseJSON = genericParseJSON jsonOptions


data Overwrite = Overwrite
  { id    :: Snowflake Overwrite
  , type_ :: Text
  , allow :: Word64
  , deny  :: Word64
  } deriving (Eq, Show, Generic)

instance ToJSON Overwrite where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Overwrite where
  parseJSON = genericParseJSON jsonOptions


data Reaction = Reaction
  { userID    :: Snowflake User
  , channelID :: Snowflake Channel
  , messageID :: Snowflake Message
  , guildID   :: Maybe (Snowflake Guild)
  , emoji     :: Emoji
  } deriving (Eq, Show, Generic)

instance ToJSON Reaction where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Reaction where
  parseJSON = genericParseJSON jsonOptions


data StatusType
  = Idle
  | DND
  | Online
  | Offline
  deriving (Eq, Show, Enum, Generic)

instance ToJSON StatusType

instance FromJSON StatusType


data Presence = Presence
  { user       :: User -- TODO: partial user
  , roles      :: [Snowflake Role]
  , game       :: Maybe Activity
  , guildID    :: Snowflake Guild
  , status     :: StatusType
  , activities :: [Activity]
  } deriving (Eq, Show, Generic)

instance ToJSON Presence where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Presence where
  parseJSON = genericParseJSON jsonOptions

data ActivityType
  = Game
  | Streaming
  | Listening
  deriving (Eq, Generic, Show, Enum)

instance ToJSON ActivityType where
  toJSON t = Number $ fromIntegral (fromEnum t)

instance FromJSON ActivityType where
  parseJSON = withScientific "ActivityType"  $ \n ->
    case toBoundedInteger n of
      Just v  -> return $ toEnum v
      Nothing -> fail $ "Invalid ActivityType: " ++ show n


data Activity = Activity
  { name          :: Text
  , type_         :: ActivityType
  , url           :: Maybe Text
  , timestamps    :: Maybe ActivityTimestamps
  , applicationID :: Maybe (Snowflake ())
  , details       :: Maybe Text
  , state         :: Maybe Text
  , party         :: Maybe ActivityParty
  , assets        :: Maybe ActivityAssets
  , secrets       :: Maybe ActivitySecrets
  , instance_     :: Bool
  , flags         :: Word64
  } deriving (Eq, Show, Generic)

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
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivityTimestamps where
  parseJSON = genericParseJSON jsonOptions


data ActivityParty = ActivityParty
  { id   :: Maybe (Snowflake ActivityParty)
  , size :: Maybe (Int, Int)
  } deriving (Eq, Show, Generic)

instance ToJSON ActivityParty where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivityParty where
  parseJSON = genericParseJSON jsonOptions


data ActivityAssets = ActivityAssets
  { largeImage :: Maybe Text
  , largeText  :: Maybe Text
  , smallImage :: Maybe Text
  , smallText  :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ActivityAssets where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivityAssets where
  parseJSON = genericParseJSON jsonOptions


data ActivitySecrets = ActivitySecrets
  { join     :: Maybe Text
  , spectate :: Maybe Text
  , match    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ActivitySecrets where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ActivitySecrets where
  parseJSON = genericParseJSON jsonOptions
