-- | General data types

module YAHDL.Types.General where

import           Data.Aeson

import           YAHDL.Types.Snowflake

data Token
  = Bot ByteString
  | User ByteString
  deriving (Generic, Show)

formatToken :: Token -> ByteString
formatToken (Bot t)  = "Bot" <> t
formatToken (User t) = t

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
type User    = Value
type Channel = Value
type Guild   = Value
type Member  = Value
