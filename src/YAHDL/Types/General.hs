-- | General data types

module YAHDL.Types.General
  ( Token(..)
  , VoiceState(..)
  , User(..)
  , TextChannel(..)
  , VoiceChannel(..)
  , GuildChannel(..)
  , DMChannel(..)
  , SingleDM(..)
  , GroupDM(..)
  , Category(..)
  , Guild(..)
  , Member(..)
  , Message(..)
  , Emoji(..)
  , Role(..)
  , Reaction(..)
  , formatToken
  , rawToken
  )
where

import           Data.Aeson

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
  , channelID :: Maybe (Snowflake Channel)
  , userID    :: Snowflake User
  , member    :: Maybe Member
  , sessionID :: Text
  , deaf      :: Bool
  , mute      :: Bool
  , selfDeaf  :: Bool
  , selfMute  :: Bool
  , suppress  :: Bool
  } deriving (Show, Generic)

instance ToJSON VoiceState where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON VoiceState where
  parseJSON = genericParseJSON jsonOptions

-- TODO: these types
data User = User
  { id            :: Snowflake User
  , username      :: Text
  , discriminator :: Text
  , bot           :: Maybe Bool
  , avatar        :: Maybe Text
  , mfaEnabled    :: Maybe Bool
  , verified      :: Maybe Bool
  , email         :: Maybe Text
  , flags         :: Int
  , premiumType   :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON User where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON User where
  parseJSON = genericParseJSON jsonOptions

data SingleDM = SingleDM
  { id            :: Snowflake SingleDM
  , lastMessageID :: Maybe (Snowflake Message)
  , recipients    :: [User]
  } deriving (Show, Generic)

instance ToJSON SingleDM where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SingleDM where
  parseJSON = genericParseJSON jsonOptions

data GroupDM = GroupDM
  { id            :: Snowflake GroupDM
  , ownerID       :: Snowflake User
  , lastMessageID :: Maybe (Snowflake Message)
  , icon          :: Maybe Text
  , recipients    :: [User]
  , name          :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

instance ToJSON GroupDM where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON GroupDM where
  parseJSON = genericParseJSON jsonOptions

data DMChannel
  = Single SingleDM
  | Group GroupDM
  deriving (Show, Generic)

data GuildChannel
  = GuildTextChannel TextChannel
  | GuildVoiceChannel VoiceChannel
  | GuildCategory Category
  deriving (Show, Generic)

instance FromJSON GuildChannel where
  parseJSON = withObject "GuildChannel" $ \v -> do
    chanType :: Int <- v .: "type"
    case chanType of
      0 -> GuildTextChannel <$> parseJSON v
      2 -> GuildVoiceChannel <$> parseJSON v
      4 -> GuildCategory <$> parseJSON v

instance FromJSON DMChannel where
  parseJSON = withObject "DMChannel" $ \v -> do
    chanType :: Int <- v .: "type"
    case chanType of
      1 -> SingleDM <$> parseJSON v
      3 -> GroupDM <$> parseJSON v

data Category = Category
  { id                   :: Snowflake Category
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , nsfw                 :: Bool
  , position             :: Int
  , guildID              :: Snowflake Guild
  } deriving (Show, Generic)

data TextChannel = TextChannel
  { id                   :: Snowflake TextChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , topic                :: Text
  , nsfw                 :: Bool
  , lastMessageID        :: Maybe (Snowflake Message)
  , rateLimitPerUser     :: Maybe Int
  , parentID             :: Maybe (Snowflake Category)
  } deriving (Show, Generic)

instance ToJSON TextChannel where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TextChannel where
  parseJSON = genericParseJSON jsonOptions

data VoiceChannel = VoiceChannel
  { id                   :: Snowflake VoiceChannel
  , guildID              :: Snowflake Guild
  , position             :: Int
  , permissionOverwrites :: [Overwrite]
  , name                 :: Text
  , bitrate              :: Int
  , userLimit            :: Int
  } deriving (Show, Generic)

instance ToJSON VoiceChannel where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON VoiceChannel where
  parseJSON = genericParseJSON jsonOptions

newtype Guild = Guild Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Member = Member Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Message = Message Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Emoji = Emoji Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Role = Role Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Overwrite = Overwrite Value
  deriving (Show, Generic, ToJSON, FromJSON)

-- Needs to have user, message and emoji
newtype Reaction = Reaction Value
  deriving (Show, Generic, ToJSON, FromJSON)
