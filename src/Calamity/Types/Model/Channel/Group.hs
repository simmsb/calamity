-- | A Group Group channel
module Calamity.Types.Model.Channel.Group
    ( GroupChannel(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Time
import           Data.Vector                          ( Vector )

data GroupChannel = GroupChannel
  { id               :: Snowflake GroupChannel
  , ownerID          :: Snowflake User
  , lastMessageID    :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , icon             :: Maybe ShortText
  , recipients       :: Vector (Snowflake User)
  , name             :: ShortText
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON GroupChannel
  deriving ( HasID GroupChannel ) via HasIDField "id" GroupChannel
  deriving ( HasID Channel ) via HasIDFieldCoerce' "id" GroupChannel
  deriving ( HasID User ) via HasIDField "ownerID" GroupChannel
