-- | General data types

module YAHDL.Types.General where

import           Data.Aeson                     ( Value
                                                , ToJSON
                                                , FromJSON
                                                )

import           YAHDL.Types.Snowflake

data VoiceState = VoiceState
  { guildID   :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake Channel)
  , userID    :: Snowflake Value
  , member    :: Maybe Value -- TODO: member object
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

-- TODO
type User = Value
type Channel = Value
type Guild = Value
