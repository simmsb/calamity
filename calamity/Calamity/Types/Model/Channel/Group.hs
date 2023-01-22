{-# LANGUAGE TemplateHaskell #-}

-- | A Group Group channel
module Calamity.Types.Model.Channel.Group (GroupChannel (..)) where

import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Time
import Optics.TH
import TextShow qualified

data GroupChannel = GroupChannel
  { id :: Snowflake GroupChannel
  , ownerID :: Snowflake User
  , lastMessageID :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , icon :: Maybe Text
  , recipients :: [User]
  , name :: Text
  }
  deriving (Show, Eq)
  deriving (TextShow.TextShow) via TextShow.FromStringShow GroupChannel
  deriving (HasID GroupChannel) via HasIDField "id" GroupChannel
  deriving (HasID Channel) via HasIDFieldCoerce' "id" GroupChannel
  deriving (HasID User) via HasIDField "ownerID" GroupChannel

$(makeFieldLabelsNoPrefix ''GroupChannel)

instance Aeson.FromJSON GroupChannel where
  parseJSON = Aeson.withObject "GroupChannel" $ \v ->
    GroupChannel
      <$> v .: "id"
      <*> v .: "owner_id"
      <*> v .:? "last_message_id"
      <*> v .:? "last_pin_timestamp"
      <*> v .:? "icon"
      <*> v .: "recipients"
      <*> v .: "name"
