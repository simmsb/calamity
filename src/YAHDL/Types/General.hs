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
  , asSpecific
  )
where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Generics.Product.Fields
import           Data.Scientific
import           Data.Typeable

import           YAHDL.Types.ISO8601
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
  pure $ (a', b')

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
  , flags         :: Maybe Int
  , premiumType   :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON User where
  parseJSON = genericParseJSON jsonOptions

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


-- | Prism typeclass for converting between the generic channel type and specialised channel types
class FromChannel a where
  -- | Convert from a channel into a more specific channel type
  fromChannel :: Channel -> Either Text a

  -- | Provide a guild ID to a channel when converting to a more specific type
  fromChannelWithGuildID :: Snowflake Guild -> Channel -> Either Text a
  fromChannelWithGuildID guildID channel = channel
    & field @"guildID" ?~ guildID
    & fromChannel

  -- | Convert from a specific channel type back to the generic channel type
  toChannel :: a -> Channel

-- | Prism for viewing a generic channel as a specific channel
asSpecific :: FromChannel a => Prism' Channel a
asSpecific = prism' toChannel (rightToMaybe . fromChannel)

toEncodingChannel :: FromChannel a => a -> Encoding
toEncodingChannel = toEncoding . toChannel

parseJSONChannel :: forall a. (FromChannel a, Typeable a) => Value -> Parser a
parseJSONChannel c = do
  parsed <- parseJSON c
  case fromChannel parsed of
    Right c' -> pure c'
    Left e -> fail $ "Failed converting channel to required type: "+||typeOf (Proxy @a)||+", data: "+||parsed||+", error: "+|e|+""

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
  fromChannel c = do
    ensureChannelType (c ^. field @"type_") DMType
    recipients <- ensureField "recipients" $ c ^. field @"recipients"
    pure $ SingleDM (coerceSnowflake $ c ^. field @"id") (c ^. field @"lastMessageID") recipients

  toChannel SingleDM {id, lastMessageID, recipients} = defChannel id DMType
    & field @"lastMessageID" .~ lastMessageID
    & field @"recipients"    ?~ recipients

instance ToJSON SingleDM where
  toEncoding = toEncodingChannel

instance FromJSON SingleDM where
  parseJSON = parseJSONChannel

data GroupDM = GroupDM
  { id            :: Snowflake GroupDM
  , ownerID       :: Snowflake User
  , lastMessageID :: Maybe (Snowflake Message)
  , icon          :: Maybe Text
  , recipients    :: SnowflakeMap User
  , name          :: Text
  } deriving (Show, Eq, Generic)

instance FromChannel GroupDM where
  fromChannel c = do
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

instance ToJSON GroupDM where
  toEncoding = toEncodingChannel

instance FromJSON GroupDM where
  parseJSON = parseJSONChannel

data DMChannel
  = Single SingleDM
  | Group GroupDM
  deriving (Show, Eq, Generic)

instance FromChannel DMChannel where
  fromChannel c@Channel {type_} = case type_ of
    DMType      -> Single <$> fromChannel c
    GroupDMType -> Group  <$> fromChannel c
    _           -> Left "Channel was not one of DMType or GroupDMType"

  toChannel (Single dm) = toChannel dm
  toChannel (Group  dm) = toChannel dm

instance ToJSON DMChannel where
  toEncoding = toEncodingChannel

instance FromJSON DMChannel where
  parseJSON = parseJSONChannel

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  deriving (Show, Eq, Generic)

instance FromChannel GuildChannel where
  fromChannel c@Channel {type_} = case type_ of
    GuildTextType     -> GuildTextChannel  <$> fromChannel c
    GuildVoiceType    -> GuildVoiceChannel <$> fromChannel c
    GuildCategoryType -> GuildCategory     <$> fromChannel c
    _                 -> Left "Channel was not one of GuildTextType, GuildVoiceType, or GuildCategoryType"

  toChannel (GuildTextChannel  c) = toChannel c
  toChannel (GuildVoiceChannel c) = toChannel c
  toChannel (GuildCategory     c) = toChannel c

instance ToJSON GuildChannel where
  toEncoding = toEncodingChannel

instance FromJSON GuildChannel where
  parseJSON = parseJSONChannel

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
  fromChannel c = do
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

instance ToJSON Category where
  toEncoding = toEncodingChannel

instance FromJSON Category where
  parseJSON = parseJSONChannel

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
  fromChannel c = do
    ensureChannelType (c ^. field @"type_") GuildTextType
    let id               =  (coerceSnowflake $ c ^. field @"id")
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

instance ToJSON TextChannel where
  toEncoding = toEncodingChannel

instance FromJSON TextChannel where
  parseJSON = parseJSONChannel

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
  fromChannel c = do
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

instance ToJSON VoiceChannel where
  toEncoding = toEncodingChannel

instance FromJSON VoiceChannel where
  parseJSON = parseJSONChannel


data Guild = Guild
  { id                          :: Snowflake Guild
  , name                        :: Text
  , icon                        :: Maybe Text
  , splash                      :: Maybe Text
  , owner                       :: Maybe Bool
  , ownerID                     :: Snowflake User
  , permissions                 :: Int
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
  , channels                    :: SnowflakeMap GuildChannel
  , presences                   :: [Presence]
  } deriving (Eq, Show, Generic)

instance ToJSON Guild where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Guild where
  parseJSON = withObject "guild" $ \v -> do
    id <- v .: "id"
    channels :: [Channel] <- v .: "channels"

    channels' <- case for channels (fromChannelWithGuildID id) of
      Right a -> pure a
      Left e -> fail $ "Error parsing guild_create channel: "+|e|+""

    Guild id <$> v .: "name"
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
             <*> (pure $ SH.fromList channels')
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
  , color       :: Maybe Int
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
    <$> v .: "title"
    <*> v .: "type"
    <*> v .: "description"
    <*> v .: "url"
    <*> v .: "timestamp"
    <*> v .: "color"
    <*> v .: "footer"
    <*> v .: "image"
    <*> v .: "thumbnail"
    <*> v .: "video"
    <*> v .: "provider"
    <*> v .: "author"
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
  , dimensions :: Maybe (Int, Int) -- doesn't make sense to have only one of the width or height
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
      <$> v .: "url"
      <*> v .: "proxyUrl"
      <*> (pure $ fuseTup2 (width, height))

data EmbedThumbnail = EmbedThumbnail
  { url        :: Maybe Text
  , proxyUrl   :: Maybe Text
  , dimensions :: Maybe (Int, Int) -- doesn't make sense to have only one of the width or height
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
      <$> v .: "url"
      <*> v .: "proxyUrl"
      <*> (pure $ fuseTup2 (width, height))

data EmbedVideo = EmbedVideo
  { url        :: Maybe Text
  , dimensions :: Maybe (Int, Int) -- doesn't make sense to have only one of the width or height
  } deriving (Eq, Show, Generic)

instance ToJSON EmbedVideo where
  toEncoding EmbedVideo{url, dimensions = Just (width, height)} =
    pairs ("url" .= url <> "width" .= width <> "height" .= height)

  toEncoding EmbedVideo{url} =
    pairs ("url" .= url)

instance FromJSON EmbedVideo where
  parseJSON = withObject "EmbedVideo" $ \v -> do
    width <- v .:? "width"
    height <- v .:? "height"

    EmbedVideo
      <$> v .: "url"
      <*> (pure $ fuseTup2 (width, height))

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
  parseJSON = genericParseJSON jsonOptions

newtype Attachment = Attachment Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Emoji = Emoji Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Role = Role Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Overwrite = Overwrite Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Needs to have user, message and emoji
newtype Reaction = Reaction Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Presence = Presence Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
