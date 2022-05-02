{-# LANGUAGE TemplateHaskell #-}

module Calamity.Types.Model.Channel.Guild.Text (TextChannel (..)) where

import Calamity.Internal.SnowflakeMap (SnowflakeMap)
import {-# SOURCE #-} Calamity.Types.Model.Channel
import Calamity.Types.Model.Channel.Guild.Category
import Calamity.Types.Model.Channel.Message
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Guild.Overwrite
import Calamity.Types.Snowflake
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time
import Optics.TH
import qualified TextShow

data TextChannel = TextChannel
  { id :: Snowflake TextChannel
  , guildID :: Snowflake Guild
  , position :: Int
  , permissionOverwrites :: SnowflakeMap Overwrite
  , name :: Text
  , topic :: Maybe Text
  , nsfw :: Bool
  , lastMessageID :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , rateLimitPerUser :: Maybe Int
  , parentID :: Maybe (Snowflake Category)
  }
  deriving (Show, Eq)
  deriving (TextShow.TextShow) via TextShow.FromStringShow TextChannel
  deriving (HasID TextChannel) via HasIDField "id" TextChannel
  deriving (HasID Channel) via HasIDFieldCoerce' "id" TextChannel
  deriving (HasID Guild) via HasIDField "guildID" TextChannel

instance Aeson.FromJSON TextChannel where
  parseJSON = Aeson.withObject "TextChannel" $ \v ->
    TextChannel
      <$> v .: "id"
      <*> v .: "guild_id"
      <*> v .: "position"
      <*> v .: "permission_overwrites"
      <*> v .: "name"
      <*> v .: "topic"
      <*> v .:? "nsfw" .!= False
      <*> v .:? "last_message_id"
      <*> v .: "last_pin_timestamp"
      <*> v .: "rate_limit_per_user"
      <*> v .:? "parent_id"

$(makeFieldLabelsNoPrefix ''TextChannel)
