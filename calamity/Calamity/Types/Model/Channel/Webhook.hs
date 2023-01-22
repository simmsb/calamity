{-# LANGUAGE TemplateHaskell #-}

-- | Channel webhooks
module Calamity.Types.Model.Channel.Webhook (Webhook (..)) where

import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Optics.TH
import TextShow.TH

data Webhook = Webhook
  { id :: Snowflake Webhook
  , type_ :: Integer
  , guildID :: Maybe (Snowflake Guild)
  , channelID :: Maybe (Snowflake Channel)
  , user :: Maybe (Snowflake User)
  , name :: Text
  , avatar :: Text
  , token :: Maybe Text
  }
  deriving (Eq, Show)
  deriving (HasID Webhook) via HasIDField "id" Webhook

instance Aeson.FromJSON Webhook where
  parseJSON = Aeson.withObject "Webhook" $ \v -> do
    user <- v .:? "user"
    userID <- traverse (.: "id") user

    Webhook
      <$> v .: "id"
      <*> v .: "type"
      <*> v .:? "guild_id"
      <*> v .:? "channel_id"
      <*> pure userID
      <*> v .: "name"
      <*> v .: "avatar"
      <*> v .:? "token"

$(deriveTextShow ''Webhook)
$(makeFieldLabelsNoPrefix ''Webhook)
