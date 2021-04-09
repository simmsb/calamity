-- | Audit Log endpoints
module Calamity.HTTP.AuditLog (
  AuditLogRequest (..),
  GetAuditLogOptions (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.Utils ()
import Calamity.Types.Model.Guild
import Calamity.Types.Model.User
import Calamity.Types.Snowflake

import Control.Lens

import Data.Default.Class

import GHC.Generics

data GetAuditLogOptions = GetAuditLogOptions
  { userID :: Maybe (Snowflake User)
  , actionType :: Maybe AuditLogAction
  , before :: Maybe (Snowflake AuditLogEntry)
  , limit :: Maybe Integer
  }
  deriving (Show, Generic, Default)

data AuditLogRequest a where
  GetAuditLog :: HasID Guild g => g -> GetAuditLogOptions -> AuditLogRequest AuditLog

instance Request (AuditLogRequest a) where
  type Result (AuditLogRequest a) = a

  route (GetAuditLog (getID @Guild -> gid) _) =
    mkRouteBuilder // S "guilds" // ID @Guild // S "audit-logs"
      & giveID gid
      & buildRoute

  action (GetAuditLog _ GetAuditLogOptions{userID, actionType, before, limit}) =
    getWithP
      ( "user_id" =:? (fromSnowflake <$> userID)
          <> "action_type" =:? (fromEnum <$> actionType)
          <> "before" =:? (fromSnowflake <$> before)
          <> "limit" =:? limit
      )
