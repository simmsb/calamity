-- | A DM channel with a single person
module Calamity.Types.Model.Channel.DM
    ( DMChannel(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils              ()
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Time
import           Data.Vector.Unboxed                  ( Vector )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                     as TSG

data DMChannel = DMChannel
  { id               :: Snowflake DMChannel
  , lastMessageID    :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , recipients       :: Vector (Snowflake User)
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric DMChannel
  deriving ( FromJSON, ToJSON ) via CalamityJSON DMChannel
  deriving ( HasID DMChannel ) via HasIDField "id" DMChannel
  deriving ( HasID Channel ) via HasIDFieldCoerce' "id" DMChannel
