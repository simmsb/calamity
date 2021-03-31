module Calamity.Types.Model.Voice.VoiceState (VoiceState (..)) where

import Calamity.Internal.AesonThings
import Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake

import Data.Aeson
import Data.Text.Lazy (Text)

import GHC.Generics

import Control.DeepSeq (NFData)
import TextShow
import qualified TextShow.Generic as TSG

data VoiceState = VoiceState
  { guildID :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake VoiceChannel)
  , userID :: Snowflake User
  , sessionID :: Text
  , deaf :: Bool
  , mute :: Bool
  , selfDeaf :: Bool
  , selfMute :: Bool
  , suppress :: Bool
  }
  deriving (Show, Eq, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric VoiceState
  deriving (ToJSON, FromJSON) via CalamityJSON VoiceState
