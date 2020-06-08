-- | Guild invites
module Calamity.Types.Model.Guild.Invite
    ( Invite(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                   ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                 as TSG

data Invite = Invite
  { code                     :: Text
  , guild                    :: Maybe (Partial Guild)
  , channel                  :: Maybe (Partial Channel)
  , targetUser               :: Maybe (Snowflake User)
  , targetUserType           :: Maybe Int
  , approximatePresenceCount :: Maybe Int
  , approximateMemberCount   :: Maybe Int
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Invite
  deriving ( ToJSON ) via CalamityJSON Invite
  deriving ( FromJSON ) via WithSpecialCases '["targetUser" `ExtractFieldFrom` "id"] Invite
