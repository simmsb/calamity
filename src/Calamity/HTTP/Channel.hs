-- | Channel endpoints
module Calamity.HTTP.Channel
    ( ChannelRequest(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.HTTP.Types
import           Calamity.Types.General
import           Calamity.Types.Snowflake

import           Data.Aeson

import           Network.Wreq

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
  { accessToken :: ShortText
  , nick        :: ShortText
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON GroupDMAddRecipientOptions

data ChannelRequest a where
  CreateMessage :: HasSpecificID c Channel => c -> Text -> ChannelRequest Message
  GetChannel :: HasSpecificID c Channel => c -> ChannelRequest Channel
  ModifyChannel :: HasSpecificID c Channel => c -> ChannelUpdate -> ChannelRequest Channel
  DeleteChannel :: HasSpecificID c Channel => c -> ChannelRequest ()
  GetChannelMessages :: HasSpecificID c Channel => c -> Maybe ChannelMessagesQuery -> ChannelRequest [Message]
  GetMessage :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> ChannelRequest Message
  CreateReaction :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> RawEmoji -> ChannelRequest ()
  DeleteOwnReaction :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> RawEmoji -> ChannelRequest ()
  DeleteUserReaction :: (HasSpecificID c Channel, HasSpecificID m Message, HasSpecificID u User) => c -> m -> RawEmoji -> u -> ChannelRequest ()
  GetReactions :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> RawEmoji -> GetReactionsOptions -> ChannelRequest [User]
  DeleteAllReactions :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> ChannelRequest ()
  EditMessage :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> Maybe Text -> Maybe Embed -> ChannelRequest Message
  DeleteMessage :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> ChannelRequest ()
  BulkDeleteMessages :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> [m] -> ChannelRequest ()
  GetChannelInvites :: HasSpecificID c Channel => c -> ChannelRequest [Invite]
  CreateChannelInvite :: HasSpecificID c Channel => c -> CreateChannelInviteOptions -> ChannelRequest Invite
  EditChannelPermissions :: HasSpecificID c Channel => c -> Overwrite -> ChannelRequest ()
  DeleteChannelPermission :: (HasSpecificID c Channel, HasSpecificID o Overwrite) => c -> o -> ChannelRequest ()
  TriggerTyping :: HasSpecificID c Channel => c -> ChannelRequest ()
  GetPinnedMessages :: HasSpecificID c Channel => c -> ChannelRequest [Message]
  AddPinnedMessage :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> ChannelRequest ()
  DeletePinnedMessage :: (HasSpecificID c Channel, HasSpecificID m Message) => c -> m -> ChannelRequest ()
  GroupDMAddRecipient :: (HasSpecificID c Channel, HasSpecificID u User) => c -> u -> GroupDMAddRecipientOptions -> ChannelRequest ()
  GroupDMRemoveRecipient :: (HasSpecificID c Channel, HasSpecificID u User) => c -> u -> ChannelRequest ()


baseRoute :: Snowflake Channel -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "channels" // ID @Channel
  & giveID id

instance Request (ChannelRequest a) a where
  toRoute (CreateMessage (getID -> id) _)      = baseRoute id // S "messages" & buildRoute
  toRoute (GetChannel    (getID -> id))        = baseRoute id                 & buildRoute
  toRoute (ModifyChannel (getID -> id) _)      = baseRoute id                 & buildRoute
  toRoute (DeleteChannel (getID -> id))        = baseRoute id                 & buildRoute
  toRoute (GetChannelMessages (getID -> id) _) = baseRoute id // S "messages" & buildRoute
  toRoute (GetMessage (getID -> cid) (getID -> mid)) = baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (CreateReaction (getID -> cid) (getID -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (show emoji) // S "@me"
    & giveID mid
    & buildRoute
  toRoute (DeleteOwnReaction (getID -> cid) (getID -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (show emoji) // S "@me"
    & giveID mid
    & buildRoute
  toRoute (DeleteUserReaction (getID -> cid) (getID -> mid) emoji (getID -> uid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (show emoji) // ID @User
    & giveID mid
    & giveID uid
    & buildRoute
  toRoute (GetReactions (getID -> cid) (getID -> mid) emoji _) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (show emoji)
    & giveID mid
    & buildRoute
  toRoute (DeleteAllReactions (getID -> cid) (getID -> mid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions"
    & giveID mid
    & buildRoute
  toRoute (EditMessage (getID -> cid) (getID -> mid) _ _) =
    baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (DeleteMessage (getID -> cid) (getID -> mid)) =
    baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (BulkDeleteMessages (getID -> cid) _) =
    baseRoute cid // S "messages" // S "bulk-delete"
    & buildRoute
  toRoute (GetChannelInvites (getID -> cid)) = baseRoute cid // S "invites" & buildRoute
  toRoute (CreateChannelInvite (getID -> cid) _) = baseRoute cid // S "invites" & buildRoute
  toRoute (EditChannelPermissions (getID -> cid) (getID -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
    & giveID oid
    & buildRoute
  toRoute (DeleteChannelPermission (getID -> cid) (getID -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
    & giveID oid
    & buildRoute
  toRoute (TriggerTyping (getID -> cid)) = baseRoute cid // S "typing" & buildRoute
  toRoute (GetPinnedMessages (getID -> cid)) = baseRoute cid // S "pins" & buildRoute
  toRoute (AddPinnedMessage (getID -> cid) (getID -> mid)) = baseRoute cid // S "pins" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (DeletePinnedMessage (getID -> cid) (getID -> mid)) = baseRoute cid // S "pins" // ID @Message
    & giveID mid
    & buildRoute
  toRoute (GroupDMAddRecipient (getID -> cid) (getID -> uid) _) = baseRoute cid // S "recipients" // ID @User
    & giveID uid
    & buildRoute
  toRoute (GroupDMRemoveRecipient (getID -> cid) (getID -> uid)) = baseRoute cid // S "recipients" // ID @User
    & giveID uid
    & buildRoute

  toAction (CreateMessage _ t) = postWith' (object ["content" .= t])
  toAction (GetChannel _) = getWith
  toAction (ModifyChannel _ p) = putWith' (toJSON p)
  toAction (DeleteChannel _) = deleteWith
  toAction (GetChannelMessages _ (Just (ChannelMessagesAround (show . fromSnowflake -> a)))) = getWithP (param "around" .~ [a])
  toAction (GetChannelMessages _ (Just (ChannelMessagesBefore (show . fromSnowflake -> a)))) = getWithP (param "before" .~ [a])
  toAction (GetChannelMessages _ (Just (ChannelMessagesAfter  (show . fromSnowflake -> a)))) = getWithP (param "after"  .~ [a])
  toAction (GetChannelMessages _ (Just (ChannelMessagesLimit  (show -> a))))                 = getWithP (param "around" .~ [a])
  toAction (GetChannelMessages _ Nothing) = getWith
  toAction (GetMessage _ _) = getWith
  toAction CreateReaction {} = putEmpty
  toAction DeleteOwnReaction {} = deleteWith
  toAction DeleteUserReaction {} = deleteWith
  toAction (GetReactions _ _ _ GetReactionsOptions { before, after, limit }) = getWithP
    (param "before" .~ maybeToList (show <$> before)
     >>> param "after" .~ maybeToList (show <$> after)
     >>> param "limit" .~ maybeToList (show <$> limit))
  toAction (DeleteAllReactions _ _) = deleteWith
  toAction (EditMessage _ _ content embed) = patchWith'
    (object ["content" .= content, "embed" .= embed])
  toAction (DeleteMessage _ _) = deleteWith
  toAction (BulkDeleteMessages _ (map getID -> ids)) = postWith' (object ["messages" .= ids])
  toAction (GetChannelInvites _) = getWith
  toAction (CreateChannelInvite _ o) = postWith' (toJSON o)
  toAction (EditChannelPermissions _ o) = putWith' (toJSON o)
  toAction (DeleteChannelPermission _ _) = deleteWith
  toAction (TriggerTyping _) = postEmpty
  toAction (GetPinnedMessages _) = getWith
  toAction (AddPinnedMessage _ _) = putEmpty
  toAction (DeletePinnedMessage _ _) = deleteWith
  toAction (GroupDMAddRecipient _ _ o) = putWith' (toJSON o)
  toAction (GroupDMRemoveRecipient _ _) = deleteWith
