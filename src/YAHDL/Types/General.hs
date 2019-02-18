-- | General data types

module YAHDL.Types.General
  ( Token(..)
  , VoiceState(..)
  , User(..)
  , Channel(..)
  , DM(..)
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

newtype DM = DM Value
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Channel = Channel Value
  deriving (Show, Generic, ToJSON, FromJSON)

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

-- Needs to have user, message and emoji
newtype Reaction = Reaction Value
  deriving (Show, Generic, ToJSON, FromJSON)
