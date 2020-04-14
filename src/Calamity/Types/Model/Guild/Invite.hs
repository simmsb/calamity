-- | Guild invites
module Calamity.Types.Model.Guild.Invite
    ( Invite(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild.Guild
import {-# SOURCE #-} Calamity.Types.Model.User
import           Calamity.Types.Partial
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Vector                      ( Vector )

data Invite = Invite
  { code                     :: ShortText
  , guild                    :: Maybe (Partial Guild)
  , channel                  :: Maybe (Partial Channel)
  , targetUser               :: Maybe (Snowflake User)
  , targetUserType           :: Maybe Int
  , approximatePresenceCount :: Maybe Int
  , approximateMemberCount   :: Maybe Int
  }
  deriving ( Eq, Show, Generic )
  deriving ( ToJSON ) via CalamityJSON Invite
  deriving ( FromJSON ) via WithSpecialCases '["targetUser" `ExtractField` "id"] Invite
