-- | A User
module Calamity.Types.Model.User
    ( User(..)
    , Partial(PartialUser)
    , StatusType(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake

import           Control.Monad

import           Data.Aeson

data User = User
  { id            :: Snowflake User
  , username      :: ShortText
  , discriminator :: ShortText
  , bot           :: Maybe Bool
  , avatar        :: Maybe ShortText
  , mfaEnabled    :: Maybe Bool
  , verified      :: Maybe Bool
  , email         :: Maybe ShortText
  , flags         :: Maybe Word64
  , premiumType   :: Maybe Word64
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON User
  deriving ( HasID ) via HasIDField User

newtype instance Partial User = PartialUser
  { id :: Snowflake User
  }
  deriving ( Show, Eq, Generic )
  deriving ( ToJSON, FromJSON ) via CalamityJSON (Partial User)
  deriving ( HasID ) via HasIDFieldAlt (Partial User) User

data StatusType
  = Idle
  | DND
  | Online
  | Offline
  | Invisible
  deriving ( Eq, Show, Enum, Generic )

instance FromJSON StatusType where
  parseJSON = withText "StatusType" $ \case
    "idle"      -> pure Idle
    "dnd"       -> pure DND
    "online"    -> pure Online
    "offline"   -> pure Offline
    "invisible" -> pure Invisible
    _           -> fail "Unknown status type"

instance ToJSON StatusType where
  toJSON = String . \case
    Idle      -> "idle"
    DND       -> "dnd"
    Online    -> "online"
    Offline   -> "offline"
    Invisible -> "invisible"
