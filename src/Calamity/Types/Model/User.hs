-- | A User
module Calamity.Types.Model.User (
  User (..),
  Partial (PartialUser),
  StatusType (..),
) where

import Calamity.Internal.AesonThings
import {-# SOURCE #-} Calamity.Types.Model.Guild.Member
import Calamity.Types.Partial
import Calamity.Types.Snowflake

import Data.Aeson
import Data.Text.Lazy (Text)
import Data.Word

import Control.DeepSeq
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data User = User
  { id :: Snowflake User
  , username :: Text
  , discriminator :: Text
  , bot :: Maybe Bool
  , avatar :: Maybe Text
  , mfaEnabled :: Maybe Bool
  , verified :: Maybe Bool
  , email :: Maybe Text
  , flags :: Maybe Word64
  , premiumType :: Maybe Word64
  }
  deriving (Show, Eq, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric User
  deriving (ToJSON, FromJSON) via CalamityJSON User
  deriving (HasID User) via HasIDField "id" User
  deriving (HasID Member) via HasIDFieldCoerce' "id" User

newtype instance Partial User = PartialUser
  { id :: Snowflake User
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via TSG.FromGeneric (Partial User)
  deriving (ToJSON, FromJSON) via CalamityJSON (Partial User)
  deriving (HasID User) via HasIDField "id" (Partial User)

data StatusType
  = Idle
  | DND
  | Online
  | Offline
  | Invisible
  deriving (Eq, Show, Enum, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric StatusType

instance FromJSON StatusType where
  parseJSON = withText "StatusType" $ \case
    "idle" -> pure Idle
    "dnd" -> pure DND
    "online" -> pure Online
    "offline" -> pure Offline
    "invisible" -> pure Invisible
    _ -> fail "Unknown status type"

instance ToJSON StatusType where
  toJSON =
    String . \case
      Idle -> "idle"
      DND -> "dnd"
      Online -> "online"
      Offline -> "offline"
      Invisible -> "invisible"
