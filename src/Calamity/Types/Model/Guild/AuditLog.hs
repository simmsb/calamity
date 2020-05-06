-- | Audit Log models
module Calamity.Types.Model.Guild.AuditLog
    ( AuditLog(..)
    , AuditLogEntry(..)
    , AuditLogEntryInfo(..)
    , AuditLogChange(..)
    , AuditLogAction(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Internal.SnowflakeMap ( SnowflakeMap )
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Scientific
import           Data.Text.Lazy                 ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic               as TSG

data AuditLog = AuditLog
  { webhooks        :: SnowflakeMap Webhook
  , users           :: SnowflakeMap User
  , auditLogEntries :: SnowflakeMap AuditLogEntry
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric AuditLog
  deriving ( FromJSON ) via CalamityJSON AuditLog

data AuditLogEntry = AuditLogEntry
  { targetID   :: Maybe (Snowflake ())
  , changes    :: [AuditLogChange]
  , userID     :: Snowflake User
  , id         :: Snowflake AuditLogEntry
  , actionType :: AuditLogAction
  , options    :: Maybe AuditLogEntryInfo
  , reason     :: Maybe Text
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric AuditLogEntry
  deriving ( FromJSON ) via CalamityJSON AuditLogEntry
  deriving ( HasID User ) via HasIDField "userID" AuditLogEntry
  deriving ( HasID AuditLogEntry ) via HasIDField "id" AuditLogEntry

data AuditLogEntryInfo = AuditLogEntryInfo
  { deleteMemberDays :: Maybe Text
  , membersRemoved   :: Maybe Text
  , channelID        :: Maybe (Snowflake Channel)
  , messageID        :: Maybe (Snowflake Message)
  , count            :: Maybe Text
  , id               :: Maybe (Snowflake ())
  , type_            :: Maybe Text
  , roleName         :: Maybe Text
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric AuditLogEntryInfo
  deriving ( FromJSON ) via CalamityJSON AuditLogEntryInfo

data AuditLogChange = AuditLogChange
  { newValue :: Maybe Value
  , oldValue :: Maybe Value
  , key      :: Text
  }
  deriving ( Show, Generic )
  deriving ( TextShow ) via FromStringShow AuditLogChange
  deriving ( FromJSON ) via CalamityJSON AuditLogChange

data AuditLogAction
  = GUILD_UPDATE
  | CHANNEL_CREATE
  | CHANNEL_UPDATE
  | CHANNEL_DELETE
  | CHANNEL_OVERWRITE_CREATE
  | CHANNEL_OVERWRITE_UPDATE
  | CHANNEL_OVERWRITE_DELETE
  | MEMBER_KICK
  | MEMBER_PRUNE
  | MEMBER_BAN_ADD
  | MEMBER_BAN_REMOVE
  | MEMBER_UPDATE
  | MEMBER_ROLE_UPDATE
  | MEMBER_MOVE
  | MEMBER_DISCONNECT
  | BOT_ADD
  | ROLE_CREATE
  | ROLE_UPDATE
  | ROLE_DELETE
  | INVITE_CREATE
  | INVITE_UPDATE
  | INVITE_DELETE
  | WEBHOOK_CREATE
  | WEBHOOK_UPDATE
  | WEBHOOK_DELETE
  | EMOJI_CREATE
  | EMOJI_UPDATE
  | EMOJI_DELETE
  | MESSAGE_DELETE
  | MESSAGE_BULK_DELETE
  | MESSAGE_PIN
  | MESSAGE_UNPIN
  | INTEGRATION_CREATE
  | INTEGRATION_UPDATE
  | INTEGRATION_DELETE
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric AuditLogAction

instance FromJSON AuditLogAction where
  parseJSON = withScientific "AuditLogAction" $ \n -> case toBoundedInteger @Int n of
    Just v  -> case v of
      1  -> pure GUILD_UPDATE
      10 -> pure CHANNEL_CREATE
      11 -> pure CHANNEL_UPDATE
      12 -> pure CHANNEL_DELETE
      13 -> pure CHANNEL_OVERWRITE_CREATE
      14 -> pure CHANNEL_OVERWRITE_UPDATE
      15 -> pure CHANNEL_OVERWRITE_DELETE
      20 -> pure MEMBER_KICK
      21 -> pure MEMBER_PRUNE
      22 -> pure MEMBER_BAN_ADD
      23 -> pure MEMBER_BAN_REMOVE
      24 -> pure MEMBER_UPDATE
      25 -> pure MEMBER_ROLE_UPDATE
      26 -> pure MEMBER_MOVE
      27 -> pure MEMBER_DISCONNECT
      28 -> pure BOT_ADD
      30 -> pure ROLE_CREATE
      31 -> pure ROLE_UPDATE
      32 -> pure ROLE_DELETE
      40 -> pure INVITE_CREATE
      41 -> pure INVITE_UPDATE
      42 -> pure INVITE_DELETE
      50 -> pure WEBHOOK_CREATE
      51 -> pure WEBHOOK_UPDATE
      52 -> pure WEBHOOK_DELETE
      60 -> pure EMOJI_CREATE
      61 -> pure EMOJI_UPDATE
      62 -> pure EMOJI_DELETE
      72 -> pure MESSAGE_DELETE
      73 -> pure MESSAGE_BULK_DELETE
      74 -> pure MESSAGE_PIN
      75 -> pure MESSAGE_UNPIN
      80 -> pure INTEGRATION_CREATE
      81 -> pure INTEGRATION_UPDATE
      82 -> pure INTEGRATION_DELETE
      _  -> fail $ "Invalid AuditLogAction: " <> show n
    Nothing -> fail $ "Invalid AuditLogAction: " <> show n

instance ToJSON AuditLogAction where
  toJSON x = toJSON @Int $ case x of
    GUILD_UPDATE             -> 1
    CHANNEL_CREATE           -> 10
    CHANNEL_UPDATE           -> 11
    CHANNEL_DELETE           -> 12
    CHANNEL_OVERWRITE_CREATE -> 13
    CHANNEL_OVERWRITE_UPDATE -> 14
    CHANNEL_OVERWRITE_DELETE -> 15
    MEMBER_KICK              -> 20
    MEMBER_PRUNE             -> 21
    MEMBER_BAN_ADD           -> 22
    MEMBER_BAN_REMOVE        -> 23
    MEMBER_UPDATE            -> 24
    MEMBER_ROLE_UPDATE       -> 25
    MEMBER_MOVE              -> 26
    MEMBER_DISCONNECT        -> 27
    BOT_ADD                  -> 28
    ROLE_CREATE              -> 30
    ROLE_UPDATE              -> 31
    ROLE_DELETE              -> 32
    INVITE_CREATE            -> 40
    INVITE_UPDATE            -> 41
    INVITE_DELETE            -> 42
    WEBHOOK_CREATE           -> 50
    WEBHOOK_UPDATE           -> 51
    WEBHOOK_DELETE           -> 52
    EMOJI_CREATE             -> 60
    EMOJI_UPDATE             -> 61
    EMOJI_DELETE             -> 62
    MESSAGE_DELETE           -> 72
    MESSAGE_BULK_DELETE      -> 73
    MESSAGE_PIN              -> 74
    MESSAGE_UNPIN            -> 75
    INTEGRATION_CREATE       -> 80
    INTEGRATION_UPDATE       -> 81
    INTEGRATION_DELETE       -> 82
