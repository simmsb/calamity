-- | Audit Log endpoints
module Calamity.HTTP.AuditLog
    ( AuditLogRequest(..)
    , GetAuditLogOptions(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Internal.Utils        ()
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Default.Class
import           Data.Function

import           GHC.Generics

import           Network.Wreq

data GetAuditLogOptions = GetAuditLogOptions
  { userID     :: Maybe (Snowflake User)
  , actionType :: Maybe AuditLogAction
  , before     :: Maybe (Snowflake AuditLogEntry)
  , limit      :: Maybe Integer
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON GetAuditLogOptions

data AuditLogRequest a where
  GetAuditLog :: HasID Guild g => g -> AuditLogRequest AuditLog

instance Request (AuditLogRequest a) where
  type Result (AuditLogRequest a) = a

  route (GetAuditLog (getID @Guild -> gid)) = mkRouteBuilder // S "guilds" // ID @Guild // S "audit-logs"
    & giveID gid
    & buildRoute

  action (GetAuditLog _) = getWith
