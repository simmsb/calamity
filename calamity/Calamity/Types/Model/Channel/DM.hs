{-# LANGUAGE TemplateHaskell #-}

-- | A DM channel with a single person
module Calamity.Types.Model.Channel.DM (DMChannel (..)) where

import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Time
import Optics.TH
import qualified TextShow

data DMChannel = DMChannel
  { id :: Snowflake DMChannel
  , lastMessageID :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , recipients :: [User]
  }
  deriving (Show, Eq)
  deriving (TextShow.TextShow) via TextShow.FromStringShow DMChannel
  deriving (HasID DMChannel) via HasIDField "id" DMChannel
  deriving (HasID Channel) via HasIDFieldCoerce' "id" DMChannel

instance Aeson.FromJSON DMChannel where
  parseJSON = Aeson.withObject "DMChannel" $ \v ->
    DMChannel
      <$> v .: "id"
      <*> v .:? "last_pin_timestamp"
      <*> v .:? "last_message_id"
      <*> v .: "recipients"

$(makeFieldLabelsNoPrefix ''DMChannel)
