-- | User presences
module Calamity.Types.Model.Presence.Presence (
  Presence (..),
  ClientStatus (..),
) where

import Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import Calamity.Types.Model.Presence.Activity
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Presence = Presence
  { user :: Snowflake User
  , guildID :: Snowflake Guild
  , status :: StatusType
  , activities :: [Activity]
  , clientStatus :: ClientStatus
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric Presence
  deriving
    (FromJSON)
    via WithSpecialCases
          '["user" `ExtractFieldFrom` "id"]
          Presence
  deriving (HasID User) via HasIDField "user" Presence
  deriving (HasID Guild) via HasIDField "guildID" Presence

data ClientStatus = ClientStatus
  { desktop :: Maybe Text
  , mobile :: Maybe Text
  , web :: Maybe Text
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric ClientStatus
  deriving (ToJSON, FromJSON) via CalamityJSON ClientStatus
