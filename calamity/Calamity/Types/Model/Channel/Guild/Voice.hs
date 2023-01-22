{-# LANGUAGE TemplateHaskell #-}

-- | Voice channels
module Calamity.Types.Model.Channel.Guild.Voice (VoiceChannel (..)) where

import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import {-# SOURCE #-} Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.Guild.Category
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Overwrite
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

data VoiceChannel = VoiceChannel
  { id :: Snowflake VoiceChannel
  , guildID :: Snowflake Guild
  , position :: Int
  , permissionOverwrites :: SnowflakeMap Overwrite
  , name :: Text
  , bitrate :: Int
  , userLimit :: Int
  , parentID :: Maybe (Snowflake Category)
  }
  deriving (Show, Eq)
  deriving (HasID VoiceChannel) via HasIDField "id" VoiceChannel
  deriving (HasID Channel) via HasIDFieldCoerce' "id" VoiceChannel
  deriving (HasID Guild) via HasIDField "guildID" VoiceChannel

instance Aeson.FromJSON VoiceChannel where
  parseJSON = Aeson.withObject "TextChannel" $ \v ->
    VoiceChannel
      <$> v .: "id"
      <*> v .: "guild_id"
      <*> v .: "position"
      <*> v .: "permission_overwrites"
      <*> v .: "name"
      <*> v .: "bitrate"
      <*> v .: "user_limit"
      <*> v .:? "parent_id"

$(deriveTextShow ''VoiceChannel)
$(makeFieldLabelsNoPrefix ''VoiceChannel)
