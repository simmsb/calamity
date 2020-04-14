module Calamity.Types.Model.Voice.VoiceState
    ( VoiceState(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.SnowflakeMap
import           Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import {-# SOURCE #-} Calamity.Types.Model.Guild.Member
import {-# SOURCE #-} Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson

data VoiceState = VoiceState
  { guildID   :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake VoiceChannel)
  , userID    :: Snowflake User
  , sessionID :: Text
  , deaf      :: Bool
  , mute      :: Bool
  , selfDeaf  :: Bool
  , selfMute  :: Bool
  , suppress  :: Bool
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON VoiceState
