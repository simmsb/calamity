-- | User presences
module Calamity.Types.Model.Presence.Presence
    ( Presence(..)
    , ClientStatus(..) ) where

import           Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.Presence.Activity
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import qualified Data.Override                          as O
import           Data.Override.Aeson                    ()

data Presence = Presence
  { user         :: Snowflake User
  , game         :: Maybe Activity
  , guildID      :: Snowflake Guild
  , status       :: StatusType
  , clientStatus :: ClientStatus
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON
      (O.Override Presence '["user" `O.As` Partial User])
  deriving ( HasID User ) via HasIDField "user" Presence
  deriving ( HasID Guild ) via HasIDField "guildID" Presence

data ClientStatus = ClientStatus
  { desktop :: Maybe ShortText
  , mobile  :: Maybe ShortText
  , web     :: Maybe ShortText
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON ClientStatus
