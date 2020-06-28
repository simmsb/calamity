-- | Channel endpoints
module Calamity.HTTP.Channel
    ( ChannelRequest(..)
    , CreateMessageOptions(..)
    , ChannelUpdate(..)
    , AllowedMentionType(..)
    , AllowedMentions(..)
    , ChannelMessagesQuery(..)
    , GetReactionsOptions(..)
    , CreateChannelInviteOptions(..)
    , GroupDMAddRecipientOptions(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Arrow
import           Control.Lens                   hiding ( (.=) )

import           Data.Aeson
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Default.Class
import           Data.Generics.Product.Subtype  ( upcast )
import           Data.Maybe
import           Data.Text                      ( Text )

import           GHC.Generics

import           Network.Wreq ( partLBS )
import           Network.Wreq.Lens
import           Network.Wreq.Session

import           TextShow

data CreateMessageOptions = CreateMessageOptions
  { content         :: Maybe Text
  , nonce           :: Maybe Text
  , tts             :: Maybe Bool
  , file            :: Maybe ByteString
  , embed           :: Maybe Embed
  , allowedMentions :: Maybe AllowedMentions
  }
  deriving ( Show, Generic, Default )

data AllowedMentionType
  = AllowedMentionRoles
  | AllowedMentionUsers
  | AllowedMentionEveryone
  deriving ( Show, Generic )

instance ToJSON AllowedMentionType where
  toJSON AllowedMentionRoles = String "roles"
  toJSON AllowedMentionUsers = String "users"
  toJSON AllowedMentionEveryone = String "everyone"

data AllowedMentions = AllowedMentions
  { parse :: [AllowedMentionType]
  , roles :: [Snowflake Role]
  , users :: [Snowflake User]
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON AllowedMentions

instance Semigroup AllowedMentions where
  AllowedMentions p0 r0 u0 <> AllowedMentions p1 r1 u1 =
    AllowedMentions (p0 <> p1) (r0 <> r1) (u0 <> u1)

instance Monoid AllowedMentions where
  mempty = def

data CreateMessageJson = CreateMessageJson
  { content :: Maybe Text
  , nonce   :: Maybe Text
  , tts     :: Maybe Bool
  , embed   :: Maybe Embed
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON CreateMessageJson

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
  deriving ( Generic, Show, Default )
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
  deriving ( Show, Generic, Default )

data CreateChannelInviteOptions = CreateChannelInviteOptions
  { maxAge    :: Maybe Int
  , maxUses   :: Maybe Int
  , temporary :: Maybe Bool
  , unique    :: Maybe Bool
  }
  deriving ( Show, Generic, Default )
  deriving ( ToJSON ) via CalamityJSON CreateChannelInviteOptions

data GroupDMAddRecipientOptions = GroupDMAddRecipientOptions
  { accessToken :: Text
  , nick        :: Text
  }
  deriving ( Show, Generic )
  deriving ( ToJSON ) via CalamityJSON GroupDMAddRecipientOptions

data ChannelRequest a where
  CreateMessage            :: (HasID Channel c) =>                                c -> CreateMessageOptions ->                 ChannelRequest Message
  GetMessage               :: (HasID Channel c, HasID Message m) =>               c -> m ->                                    ChannelRequest Message
  EditMessage              :: (HasID Channel c, HasID Message m) =>               c -> m -> Maybe Text -> Maybe Embed ->       ChannelRequest Message
  DeleteMessage            :: (HasID Channel c, HasID Message m) =>               c -> m ->                                    ChannelRequest ()
  BulkDeleteMessages       :: (HasID Channel c, HasID Message m) =>               c -> [m] ->                                  ChannelRequest ()
  GetChannel               :: (HasID Channel c) =>                                c ->                                         ChannelRequest Channel
  ModifyChannel            :: (HasID Channel c) =>                                c -> ChannelUpdate ->                        ChannelRequest Channel
  DeleteChannel            :: (HasID Channel c) =>                                c ->                                         ChannelRequest ()
  GetChannelMessages       :: (HasID Channel c) =>                                c -> Maybe ChannelMessagesQuery ->           ChannelRequest [Message]
  CreateReaction           :: (HasID Channel c, HasID Message m) =>               c -> m -> RawEmoji ->                        ChannelRequest ()
  DeleteOwnReaction        :: (HasID Channel c, HasID Message m) =>               c -> m -> RawEmoji ->                        ChannelRequest ()
  DeleteUserReaction       :: (HasID Channel c, HasID Message m, HasID User u) => c -> m -> RawEmoji -> u ->                   ChannelRequest ()
  GetReactions             :: (HasID Channel c, HasID Message m) =>               c -> m -> RawEmoji -> GetReactionsOptions -> ChannelRequest [User]
  DeleteAllReactions       :: (HasID Channel c, HasID Message m) =>               c -> m ->                                    ChannelRequest ()
  GetChannelInvites        :: (HasID Channel c) =>                                c ->                                         ChannelRequest [Invite]
  CreateChannelInvite      :: (HasID Channel c) =>                                c -> CreateChannelInviteOptions ->           ChannelRequest Invite
  EditChannelPermissions   :: (HasID Channel c) =>                                c -> Overwrite ->                            ChannelRequest ()
  DeleteChannelPermission  :: (HasID Channel c, HasID Overwrite o) =>             c -> o ->                                    ChannelRequest ()
  TriggerTyping            :: (HasID Channel c) =>                                c ->                                         ChannelRequest ()
  GetPinnedMessages        :: (HasID Channel c) =>                                c ->                                         ChannelRequest [Message]
  AddPinnedMessage         :: (HasID Channel c, HasID Message m) =>               c -> m ->                                    ChannelRequest ()
  DeletePinnedMessage      :: (HasID Channel c, HasID Message m) =>               c -> m ->                                    ChannelRequest ()
  GroupDMAddRecipient      :: (HasID Channel c, HasID User u) =>                  c -> u -> GroupDMAddRecipientOptions ->      ChannelRequest ()
  GroupDMRemoveRecipient   :: (HasID Channel c, HasID User u) =>                  c -> u ->                                    ChannelRequest ()


baseRoute :: Snowflake Channel -> RouteBuilder _
baseRoute id = mkRouteBuilder // S "channels" // ID @Channel
  & giveID id

instance Request (ChannelRequest a) where
  type Result (ChannelRequest a) = a

  route (CreateMessage (getID -> id) _) = baseRoute id // S "messages"
    & buildRoute
  route (GetChannel (getID -> id)) = baseRoute id
    & buildRoute
  route (ModifyChannel (getID -> id) _) = baseRoute id
    & buildRoute
  route (DeleteChannel (getID -> id)) = baseRoute id
    & buildRoute
  route (GetChannelMessages (getID -> id) _) = baseRoute id // S "messages"
    & buildRoute
  route (GetMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  route (CreateReaction (getID -> cid) (getID @Message -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji) // S "@me"
    & giveID mid
    & buildRoute
  route (DeleteOwnReaction (getID -> cid) (getID @Message -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji) // S "@me"
    & giveID mid
    & buildRoute
  route (DeleteUserReaction (getID -> cid) (getID @Message -> mid) emoji (getID @User -> uid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji) // ID @User
    & giveID mid
    & giveID uid
    & buildRoute
  route (GetReactions (getID -> cid) (getID @Message -> mid) emoji _) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // S (showt emoji)
    & giveID mid
    & buildRoute
  route (DeleteAllReactions (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions"
    & giveID mid
    & buildRoute
  route (EditMessage (getID -> cid) (getID @Message -> mid) _ _) = baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  route (DeleteMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "messages" // ID @Message
    & giveID mid
    & buildRoute
  route (BulkDeleteMessages (getID -> cid) _) = baseRoute cid // S "messages" // S "bulk-delete"
    & buildRoute
  route (GetChannelInvites (getID -> cid)) = baseRoute cid // S "invites"
    & buildRoute
  route (CreateChannelInvite (getID -> cid) _) = baseRoute cid // S "invites"
    & buildRoute
  route (EditChannelPermissions (getID -> cid) (getID @Overwrite -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
    & giveID oid
    & buildRoute
  route (DeleteChannelPermission (getID -> cid) (getID @Overwrite -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
    & giveID oid
    & buildRoute
  route (TriggerTyping (getID -> cid)) = baseRoute cid // S "typing"
    & buildRoute
  route (GetPinnedMessages (getID -> cid)) = baseRoute cid // S "pins"
    & buildRoute
  route (AddPinnedMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "pins" // ID @Message
    & giveID mid
    & buildRoute
  route (DeletePinnedMessage (getID -> cid) (getID @Message -> mid)) = baseRoute cid // S "pins" // ID @Message
    & giveID mid
    & buildRoute
  route (GroupDMAddRecipient (getID -> cid) (getID @User -> uid) _) = baseRoute cid // S "recipients" // ID @User
    & giveID uid
    & buildRoute
  route (GroupDMRemoveRecipient (getID -> cid) (getID @User -> uid)) = baseRoute cid // S "recipients" // ID @User
    & giveID uid
    & buildRoute

  action (CreateMessage _ o@CreateMessageOptions { file = Nothing }) = postWith'
    (toJSON . upcast @CreateMessageJson $ o)
  action (CreateMessage _ o@CreateMessageOptions { file = Just f }) = postWith'
    [partLBS @IO "file" f, partLBS "payload_json" (encode . upcast @CreateMessageJson $ o)]
  action (GetChannel _) = getWith
  action (ModifyChannel _ p) = putWith' (toJSON p)
  action (DeleteChannel _) = deleteWith
  action (GetChannelMessages _ (Just (ChannelMessagesAround (showt . fromSnowflake -> a)))) = getWithP
    (param "around" .~ [a])
  action (GetChannelMessages _ (Just (ChannelMessagesBefore (showt . fromSnowflake -> a)))) = getWithP
    (param "before" .~ [a])
  action (GetChannelMessages _ (Just (ChannelMessagesAfter (showt . fromSnowflake -> a)))) = getWithP
    (param "after" .~ [a])
  action (GetChannelMessages _ (Just (ChannelMessagesLimit (showt -> a)))) = getWithP (param "around" .~ [a])
  action (GetChannelMessages _ Nothing) = getWith
  action (GetMessage _ _) = getWith
  action CreateReaction {} = putEmpty
  action DeleteOwnReaction {} = deleteWith
  action DeleteUserReaction {} = deleteWith
  action (GetReactions _ _ _ GetReactionsOptions { before, after, limit }) = getWithP
    (param "before" .~ maybeToList (showt <$> before) >>>
     param "after" .~ maybeToList (showt <$> after) >>>
     param "limit" .~ maybeToList (showt <$> limit))
  action (DeleteAllReactions _ _) = deleteWith
  action (EditMessage _ _ content embed) = patchWith' (object ["content" .= content, "embed" .= embed])
  action (DeleteMessage _ _) = deleteWith
  action (BulkDeleteMessages _ (map (getID @Message) -> ids)) = postWith' (object ["messages" .= ids])
  action (GetChannelInvites _) = getWith
  action (CreateChannelInvite _ o) = postWith' (toJSON o)
  action (EditChannelPermissions _ o) = putWith' (toJSON o)
  action (DeleteChannelPermission _ _) = deleteWith
  action (TriggerTyping _) = postEmpty
  action (GetPinnedMessages _) = getWith
  action (AddPinnedMessage _ _) = putEmpty
  action (DeletePinnedMessage _ _) = deleteWith
  action (GroupDMAddRecipient _ _ o) = putWith' (toJSON o)
  action (GroupDMRemoveRecipient _ _) = deleteWith
