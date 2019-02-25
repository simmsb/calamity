-- | General data types

module YAHDL.Types.General
  ( Token(..)
  , VoiceState(..)
  , User(..)
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
import           YAHDL.Types.Snowflake

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
  , recipients           :: Maybe [User]
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
  fromChannel :: Channel -> Maybe a
  toChannel   :: a -> Channel

toEncodingChannel :: FromChannel a => a -> Encoding
toEncodingChannel = toEncoding . toChannel

parseJSONChannel :: forall a. (FromChannel a, Typeable a) => Value -> Parser a
parseJSONChannel c = do
  parsed <- parseJSON c
  case fromChannel parsed of
    Just c' -> pure c'
    Nothing -> fail $ "Failed converting channel to required type: "+||typeOf (Proxy @a)||+", data: "+||parsed||+""

data SingleDM = SingleDM
  { id            :: Snowflake SingleDM
  , lastMessageID :: Maybe (Snowflake Message)
  , recipients    :: [User]
  } deriving (Show, Eq, Generic)

instance FromChannel SingleDM where
  fromChannel c = do
    guard $ (c ^. field @"type_") == DMType
    recipients <- c ^. field @"recipients"
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
  , recipients    :: [User]
  , name          :: Text
  } deriving (Show, Eq, Generic)

instance FromChannel GroupDM where
  fromChannel c = do
    guard $ (c ^. field @"type_") == GroupDMType
    owner      <- c ^. field @"ownerID"
    recipients <- c ^. field @"recipients"
    name       <- c ^. field @"name"
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
    _           -> Nothing

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
    _                 -> Nothing

  toChannel (GuildTextChannel  c) = toChannel c
  toChannel (GuildVoiceChannel c) = toChannel c
  toChannel (GuildCategory     c) = toChannel c

instance ToJSON GuildChannel where
  toEncoding = toEncodingChannel

instance FromJSON GuildChannel where
  parseJSON = parseJSONChannel


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
    guard $ (c ^. field @"type_") == GuildCategoryType
    permissionOverwrites <- c ^. field @"permissionOverwrites"
    name                 <- c ^. field @"name"
    let nsfw             = fromMaybe False $ c ^. field @"nsfw"
    position             <- c ^. field @"position"
    guildID              <- c ^. field @"guildID"
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

-- TODO: able to decode channels providing the guild_id
data TextChannel = TextChannel
  { id                   :: Snowflake TextChannel
  , guildID              :: Maybe (Snowflake Guild)
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
    guard $ (c ^. field @"type_") == GuildTextType
    let id               =  (coerceSnowflake $ c ^. field @"id")
    let guildID          =  c ^. field @"guildID"
    position             <- c ^. field @"position"
    permissionOverwrites <- c ^. field @"permissionOverwrites"
    name                 <- c ^. field @"name"
    let topic            =  c ^. field @"topic"
    nsfw                 <- c ^. field @"nsfw"
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
    & field @"guildID"              .~ guildID
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
    guard $ (c ^. field @"type_") == GuildVoiceType
    guildID              <- c ^. field @"guildID"
    position             <- c ^. field @"position"
    permissionOverwrites <- c ^. field @"permissionOverwrites"
    name                 <- c ^. field @"name"
    bitrate              <- c ^. field @"bitrate"
    userLimit            <- c ^. field @"userLimit"
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


-- TODO: custom decoder to allow owner, permissions, embedEnabled, widgetEnabled etc to default
data Guild = Guild
  { id                          :: Snowflake Guild
  , name                        :: Text
  , icon                        :: Maybe Text
  , splash                      :: Maybe Text
  , owner                       :: Maybe Bool
  , ownerID                     :: Snowflake User
  , permissions                 :: Maybe Int
  , region                      :: Text
  , afkChannelID                :: Maybe (Snowflake GuildChannel)
  , afkTimeout                  :: Int
  , embedEnabled                :: Maybe Bool
  , embedChannelID              :: Maybe (Snowflake GuildChannel)
  , verificationLevel           :: Int
  , defaultMessageNotifications :: Int
  , explicitContentFilter       :: Int
  , roles                       :: [Role]
  , emojis                      :: [Emoji]
  , features                    :: [Text]
  , mfaLevel                    :: Int
  , applicationID               :: Maybe (Snowflake User)
  , widgetEnabled               :: Maybe Bool
  , widgetChannelID             :: Maybe (Snowflake GuildChannel)
  , systemChannelID             :: Maybe (Snowflake GuildChannel)
  , joinedAt                    :: Maybe ISO8601Timestamp
  , large                       :: Bool
  , unavailable                 :: Bool
  , memberCount                 :: Int
  , voiceStates                 :: [VoiceState]
  , members                     :: [Member]
  , channels                    :: [GuildChannel]
  , presences                   :: [Presence]
  } deriving (Eq, Show, Generic)

instance ToJSON Guild where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Guild where
  parseJSON = genericParseJSON jsonOptions


data UnavailableGuild = UnavailableGuild
  { id          :: Snowflake Guild
  , unavailable :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON UnavailableGuild where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON UnavailableGuild where
  parseJSON = genericParseJSON jsonOptions


newtype Member = Member Value
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Message = Message Value
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
