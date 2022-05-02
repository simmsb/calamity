{-# LANGUAGE TemplateHaskell #-}

module Calamity.Types.Model.Voice.VoiceState (VoiceState (..)) where

import Calamity.Internal.Utils
import Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

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
  deriving (Show, Eq)
  deriving (Aeson.ToJSON) via CalamityToJSON VoiceState

instance CalamityToJSON' VoiceState where
  toPairs VoiceState {..} =
    [ "guild_id" .= guildID
    , "channel_id" .= channelID
    , "user_id" .= userID
    , "session_id" .= sessionID
    , "deaf" .= deaf
    , "mute" .= mute
    , "self_deaf" .= selfDeaf
    , "self_mute" .= selfMute
    , "suppress" .= suppress
    ]

instance Aeson.FromJSON VoiceState where
  parseJSON = Aeson.withObject "VoiceState" $ \v ->
    VoiceState
      <$> v .:? "guild_id"
      <*> v .:? "channel_id"
      <*> v .: "user_id"
      <*> v .: "session_id"
      <*> v .: "deaf"
      <*> v .: "mute"
      <*> v .: "self_deaf"
      <*> v .: "self_mute"
      <*> v .: "suppress"

$(deriveTextShow ''VoiceState)
$(makeFieldLabelsNoPrefix ''VoiceState)
