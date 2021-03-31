-- | A Group Group channel
module Calamity.Types.Model.Channel.Group
    ( GroupChannel(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils              ()
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                       ( Text )
import           Data.Time
import           Data.Vector.Unboxing                 ( Vector )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                     as TSG

data GroupChannel = GroupChannel
  { id               :: Snowflake GroupChannel
  , ownerID          :: Snowflake User
  , lastMessageID    :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , icon             :: Maybe Text
  , recipients       :: Vector (Snowflake User)
  , name             :: Text
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric GroupChannel
  deriving ( ToJSON, FromJSON ) via CalamityJSON GroupChannel
  deriving ( HasID GroupChannel ) via HasIDField "id" GroupChannel
  deriving ( HasID Channel ) via HasIDFieldCoerce' "id" GroupChannel
  deriving ( HasID User ) via HasIDField "ownerID" GroupChannel
