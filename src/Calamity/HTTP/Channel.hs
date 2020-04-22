-- | Channel endpoints
module Calamity.HTTP.Channel
    ( ChannelUpdate(..)
    , ChannelMessagesQuery(..)
    , ChannelRequest(..)
    , GetReactionsOptions(..)
    , CreateChannelInviteOptions(..)
    , GroupDMAddRecipientOptions(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.Guild.Overwrite
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Arrow
import           Control.Lens                         hiding ( (.=) )

import           Data.Aeson
import           Data.Default.Class
import           Data.Maybe
import           Data.Text.Lazy                       ( Text )

import           GHC.Generics

import           Network.Wreq

import           TextShow

data ChannelUpdate = ChannelUpdate
  { name                 :: Maybe Text
  , position             :: Maybe Int
  , topic                :: Maybe Text
  , nsfw                 :: Maybe Bool
  , rateLimitPerUser     :: Maybe Int
  , bitrate              :: Maybe Int
  , userLimit            :: Maybe Int
  , permissionOverwrites :: Maybe [Overwrite]
  , parentID             :: Maybe (Snowflake Channel)
  }
  deriving ( Generic, Show )
  deriving anyclass ( Default )
  deriving ( ToJSON ) via CalamityJSON ChannelUpdate

data ChannelMessagesQuery
  = ChannelMessagesAround
      { around :: Snowflake Message
      }
  | ChannelMessagesBefore
      { before :: Snowflake Message
      }
  | ChannelMessagesAfter
      { after :: Snowflake Message
      }
  | ChannelMessagesLimit
      { limit :: Int
      }
  deriving ( Generic, Show )
  deriving ( ToJSON ) via CalamityJSON ChannelMessagesQuery

data GetReactionsOptions = GetReactionsOptions
  { before :: Maybe (Snowflake User)
  , after  :: Maybe (Snowflake User)
  , limit  :: Maybe Integer
  }
  deriving ( Show )

data CreateChannelInviteOptions = CreateChannelInviteOptions
  { maxAge    :: Maybe Int
  , maxUses   :: Maybe Int
  , temporary :: Maybe Bool
  , unique    :: Maybe Bool
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON CreateChannelInviteOptions

data GroupDMAddRecipientOptions = GroupDMAddRecipientOptions
  { accessToken :: Text
  , nick        :: Text
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON GroupDMAddRecipientOptions

data ChannelRequest a where
  CreateMessage            :: (HasID Channel c) => c -> Text -> ChannelRequest Message
  GetMessage               :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest Message
  EditMessage              :: (HasID Channel c, HasID Message m) => c -> m -> Maybe Text -> Maybe Embed -> ChannelRequest Message
  DeleteMessage            :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  BulkDeleteMessages       :: (HasID Channel c, HasID Message m) => c -> [m] -> ChannelRequest ()
  GetChannel               :: (HasID Channel c) => c -> ChannelRequest Channel
  ModifyChannel            :: (HasID Channel c) => c -> ChannelUpdate -> ChannelRequest Channel
  DeleteChannel            :: (HasID Channel c) => c -> ChannelRequest ()
  GetChannelMessages       :: (HasID Channel c) => c -> Maybe ChannelMessagesQuery -> ChannelRequest [Message]
  CreateReaction           :: (HasID Channel c, HasID Message m) => c -> m -> RawEmoji -> ChannelRequest ()
  DeleteOwnReaction        :: (HasID Channel c, HasID Message m) => c -> m -> RawEmoji -> ChannelRequest ()
  DeleteUserReaction       :: (HasID Channel c, HasID Message m, HasID User u) => c -> m -> RawEmoji -> u -> ChannelRequest ()
  GetReactions             :: (HasID Channel c, HasID Message m) => c -> m -> RawEmoji -> GetReactionsOptions -> ChannelRequest [User]
  DeleteAllReactions       :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  GetChannelInvites        :: (HasID Channel c) => c -> ChannelRequest [Invite]
  CreateChannelInvite      :: (HasID Channel c) => c -> CreateChannelInviteOptions -> ChannelRequest Invite
  EditChannelPermissions   :: (HasID Channel c) => c -> Overwrite -> ChannelRequest ()
  DeleteChannelPermission  :: (HasID Channel c, HasID Overwrite o) => c -> o -> ChannelRequest ()
  TriggerTyping            :: (HasID Channel c) => c -> ChannelRequest ()
  GetPinnedMessages        :: (HasID Channel c) => c -> ChannelRequest [Message]
  AddPinnedMessage         :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  DeletePinnedMessage      :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  GroupDMAddRecipient      :: (HasID Channel c, HasID User u) => c -> u -> GroupDMAddRecipientOptions -> ChannelRequest ()
  GroupDMRemoveRecipient   :: (HasID Channel c, HasID User u) => c -> u -> ChannelRequest ()


baseRoute :: Snowflake Channel -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "channels" // ID @Channel
  & giveID id

instance Request (ChannelRequest a) a where
  toRoute (CreateMessage (getID -> id) _)      = baseRoute id // S "messages" & buildRoute
  toRoute (GetChannel    (getID -> id))        = baseRoute id                 & buildRoute
  toRoute (ModifyChannel (getID -> id) _)      = baseRoute id                 & buildRoute
  toRoute (DeleteChannel (getID -> id))        = baseRoute id                 & buildRoute
  toRoute (GetChannelMessages (getID -> id) _) = baseRoute id // S "messages" & buildRoute
  toRoute (GetMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (CreateReaction (getID -> cid) (getID @Message -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji) // S "@me"
    & giveID mid
    & buildRoute
  toRoute (DeleteOwnReaction (getID -> cid) (getID @Message -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji) // S "@me"
    & giveID mid
    & buildRoute
  toRoute (DeleteUserReaction (getID -> cid) (getID @Message -> mid) emoji (getID @User -> uid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji) // ID @User
    & giveID mid
    & giveID uid
    & buildRoute
  toRoute (GetReactions (getID -> cid) (getID @Message -> mid) emoji _) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji)
    & giveID mid
    & buildRoute
  toRoute (DeleteAllReactions (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions"
    & giveID mid
    & buildRoute
  toRoute (EditMessage (getID -> cid) (getID @Message -> mid) _ _) =
    baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (DeleteMessage (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (BulkDeleteMessages (getID -> cid) _) =
    baseRoute cid // S "messages" // S "bulk-delete"
    & buildRoute
  toRoute (GetChannelInvites (getID -> cid)) = baseRoute cid // S "invites" & buildRoute
  toRoute (CreateChannelInvite (getID -> cid) _) = baseRoute cid // S "invites" & buildRoute
  toRoute (EditChannelPermissions (getID -> cid) (getID @Overwrite -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
    & giveID oid
    & buildRoute
  toRoute (DeleteChannelPermission (getID -> cid) (getID @Overwrite -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
    & giveID oid
    & buildRoute
  toRoute (TriggerTyping (getID -> cid)) = baseRoute cid // S "typing" & buildRoute
  toRoute (GetPinnedMessages (getID -> cid)) = baseRoute cid // S "pins" & buildRoute
  toRoute (AddPinnedMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "pins" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (DeletePinnedMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "pins" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (GroupDMAddRecipient (getID -> cid) (getID @User -> uid) _) = baseRoute cid // S "recipients" // ID @User
    & giveID uid
    & buildRoute
  toRoute (GroupDMRemoveRecipient (getID -> cid) (getID @User -> uid)) = baseRoute cid // S "recipients" // ID @User
    & giveID uid
    & buildRoute

  toAction (CreateMessage _ t) = postWith' (object ["content" .= t])
  toAction (GetChannel _)      = getWith
  toAction (ModifyChannel _ p) = putWith' (toJSON p)
  toAction (DeleteChannel _)   = deleteWith
  toAction (GetChannelMessages _ (Just (ChannelMessagesAround (showt . fromSnowflake -> a)))) = getWithP (param "around" .~ [a])
  toAction (GetChannelMessages _ (Just (ChannelMessagesBefore (showt . fromSnowflake -> a)))) = getWithP (param "before" .~ [a])
  toAction (GetChannelMessages _ (Just (ChannelMessagesAfter  (showt . fromSnowflake -> a)))) = getWithP (param "after"  .~ [a])
  toAction (GetChannelMessages _ (Just (ChannelMessagesLimit  (showt -> a))))                 = getWithP (param "around" .~ [a])
  toAction (GetChannelMessages _ Nothing) = getWith
  toAction (GetMessage _ _)               = getWith
  toAction CreateReaction {}              = putEmpty
  toAction DeleteOwnReaction {}           = deleteWith
  toAction DeleteUserReaction {}          = deleteWith
  toAction (GetReactions _ _ _ GetReactionsOptions { before, after, limit }) = getWithP
    (param "before" .~ maybeToList (showt <$> before)
     >>> param "after" .~ maybeToList (showt <$> after)
     >>> param "limit" .~ maybeToList (showt <$> limit))
  toAction (DeleteAllReactions _ _) = deleteWith
  toAction (EditMessage _ _ content embed) = patchWith'
    (object ["content" .= content, "embed" .= embed])
  toAction (DeleteMessage _ _)                       = deleteWith
  toAction (BulkDeleteMessages _ (map (getID @Message) -> ids)) = postWith' (object ["messages" .= ids])
  toAction (GetChannelInvites _)                     = getWith
  toAction (CreateChannelInvite _ o)                 = postWith' (toJSON o)
  toAction (EditChannelPermissions _ o)              = putWith' (toJSON o)
  toAction (DeleteChannelPermission _ _)             = deleteWith
  toAction (TriggerTyping _)                         = postEmpty
  toAction (GetPinnedMessages _)                     = getWith
  toAction (AddPinnedMessage _ _)                    = putEmpty
  toAction (DeletePinnedMessage _ _)                 = deleteWith
  toAction (GroupDMAddRecipient _ _ o)               = putWith' (toJSON o)
  toAction (GroupDMRemoveRecipient _ _)              = deleteWith
