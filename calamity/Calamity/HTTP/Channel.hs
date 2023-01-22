{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Channel endpoints
module Calamity.HTTP.Channel (
  ChannelRequest (..),
  CreateMessageAttachment (..),
  CreateMessageOptions (..),
  EditMessageData (..),
  editMessageContent,
  editMessageEmbeds,
  editMessageFlags,
  editMessageAllowedMentions,
  editMessageComponents,
  ChannelUpdate (..),
  AllowedMentionType (..),
  AllowedMentions (..),
  ChannelMessagesFilter (..),
  ChannelMessagesLimit (..),
  GetReactionsOptions (..),
  CreateChannelInviteOptions (..),
  GroupDMAddRecipientOptions (..),
) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Emoji (RawEmoji (..))
import Calamity.Types.Model.Guild.Invite (Invite)
import Calamity.Types.Model.Guild.Overwrite (Overwrite)
import Calamity.Types.Model.Guild.Role (Role)
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as K
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Req
import Network.Mime
import Optics
import TextShow

data CreateMessageAttachment = CreateMessageAttachment
  { filename :: Text
  , description :: Maybe Text
  , content :: ByteString
  }
  deriving (Show)

data CreateMessageOptions = CreateMessageOptions
  { content :: Maybe Text
  , nonce :: Maybe Text
  , tts :: Maybe Bool
  , attachments :: Maybe [CreateMessageAttachment]
  , embeds :: Maybe [Embed]
  , allowedMentions :: Maybe AllowedMentions
  , messageReference :: Maybe MessageReference
  , components :: Maybe [Component]
  }
  deriving (Show)

instance Default CreateMessageOptions where
  def = CreateMessageOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data CreateMessageAttachmentJson = CreateMessageAttachmentJson
  { id :: Int
  , filename :: Text
  , description :: Maybe Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateMessageAttachmentJson

instance CalamityToJSON' CreateMessageAttachmentJson where
  toPairs CreateMessageAttachmentJson {..} =
    [ "filename" .= filename
    , "description" .?= description
    , "id" .= id
    ]

data CreateMessageJson = CreateMessageJson
  { content :: Maybe Text
  , nonce :: Maybe Text
  , tts :: Maybe Bool
  , embeds :: Maybe [Embed]
  , allowedMentions :: Maybe AllowedMentions
  , messageReference :: Maybe MessageReference
  , components :: Maybe [Component]
  , attachments :: Maybe [CreateMessageAttachmentJson]
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateMessageJson

instance CalamityToJSON' CreateMessageJson where
  toPairs CreateMessageJson {..} =
    [ "content" .?= content
    , "nonce" .?= nonce
    , "tts" .?= tts
    , "embeds" .?= embeds
    , "allowed_mentions" .?= allowedMentions
    , "message_reference" .?= messageReference
    , "components" .?= components
    , "attachments" .?= attachments
    ]

data AllowedMentionType
  = AllowedMentionRoles
  | AllowedMentionUsers
  | AllowedMentionEveryone
  deriving (Show)

instance Aeson.ToJSON AllowedMentionType where
  toJSON AllowedMentionRoles = Aeson.String "roles"
  toJSON AllowedMentionUsers = Aeson.String "users"
  toJSON AllowedMentionEveryone = Aeson.String "everyone"

data AllowedMentions = AllowedMentions
  { parse :: [AllowedMentionType]
  , roles :: [Snowflake Role]
  , users :: [Snowflake User]
  , repliedUser :: Bool
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON AllowedMentions

instance CalamityToJSON' AllowedMentions where
  toPairs AllowedMentions {..} =
    [ "parse" .= parse
    , "roles" .= roles
    , "users" .= users
    , "replied_user" .= repliedUser
    ]

instance Default AllowedMentions where
  def = AllowedMentions def def def False

instance Semigroup AllowedMentions where
  AllowedMentions p0 r0 u0 ru0 <> AllowedMentions p1 r1 u1 ru1 =
    AllowedMentions (p0 <> p1) (r0 <> r1) (u0 <> u1) (ru0 || ru1)

instance Monoid AllowedMentions where
  mempty = def

{- | Parameters to the Edit Message endpoint.

 Use the provided methods (@editMessageX@) to create a value with the
 field set, use the Semigroup instance to union the values.

 ==== Examples

 >>> encode $ editMessageContent (Just "test") <> editMessageFlags Nothing
 "{\"nick\":\"test\",\"deaf\":null}"
-}
newtype EditMessageData = EditMessageData Aeson.Object
  deriving stock (Show)
  deriving newtype (Aeson.ToJSON, Semigroup, Monoid)

editMessageContent :: Maybe Text -> EditMessageData
editMessageContent v = EditMessageData $ K.fromList [("content", Aeson.toJSON v)]

editMessageEmbeds :: [Embed] -> EditMessageData
editMessageEmbeds v = EditMessageData $ K.fromList [("embeds", Aeson.toJSON v)]

editMessageFlags :: Maybe Word64 -> EditMessageData
editMessageFlags v = EditMessageData $ K.fromList [("flags", Aeson.toJSON v)]

editMessageAllowedMentions :: Maybe AllowedMentions -> EditMessageData
editMessageAllowedMentions v = EditMessageData $ K.fromList [("allowed_mentions", Aeson.toJSON v)]

editMessageComponents :: [Component] -> EditMessageData
editMessageComponents v = EditMessageData $ K.fromList [("components", Aeson.toJSON v)]

data ChannelUpdate = ChannelUpdate
  { name :: Maybe Text
  , position :: Maybe Int
  , topic :: Maybe Text
  , nsfw :: Maybe Bool
  , rateLimitPerUser :: Maybe Int
  , bitrate :: Maybe Int
  , userLimit :: Maybe Int
  , permissionOverwrites :: Maybe [Overwrite]
  , parentID :: Maybe (Snowflake Channel)
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ChannelUpdate

instance Default ChannelUpdate where
  def = ChannelUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance CalamityToJSON' ChannelUpdate where
  toPairs ChannelUpdate {..} =
    [ "name" .?= name
    , "position" .?= position
    , "topic" .?= topic
    , "nsfw" .?= nsfw
    , "rate_limit_per_user" .?= rateLimitPerUser
    , "bitrate" .?= bitrate
    , "user_limit" .?= userLimit
    , "permission_overwrites" .?= permissionOverwrites
    , "parent_id" .?= parentID
    ]

data ChannelMessagesFilter
  = ChannelMessagesAround
      { around :: Snowflake Message
      }
  | ChannelMessagesBefore
      { before :: Snowflake Message
      }
  | ChannelMessagesAfter
      { after :: Snowflake Message
      }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON ChannelMessagesFilter

instance CalamityToJSON' ChannelMessagesFilter where
  toPairs ChannelMessagesAround {around} = ["around" .= around]
  toPairs ChannelMessagesBefore {before} = ["before" .= before]
  toPairs ChannelMessagesAfter {after} = ["after" .= after]

newtype ChannelMessagesLimit = ChannelMessagesLimit
  { limit :: Integer
  }
  deriving stock (Show)

data GetReactionsOptions = GetReactionsOptions
  { before :: Maybe (Snowflake User)
  , after :: Maybe (Snowflake User)
  , limit :: Maybe Integer
  }
  deriving (Show)

instance Default GetReactionsOptions where
  def = GetReactionsOptions Nothing Nothing Nothing

data CreateChannelInviteOptions = CreateChannelInviteOptions
  { maxAge :: Maybe Int
  , maxUses :: Maybe Int
  , temporary :: Maybe Bool
  , unique :: Maybe Bool
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateChannelInviteOptions

instance Default CreateChannelInviteOptions where
  def = CreateChannelInviteOptions Nothing Nothing Nothing Nothing

instance CalamityToJSON' CreateChannelInviteOptions where
  toPairs CreateChannelInviteOptions {..} =
    [ "max_age" .?= maxAge
    , "max_uses" .?= maxUses
    , "temporary" .?= temporary
    , "unique" .?= unique
    ]

data GroupDMAddRecipientOptions = GroupDMAddRecipientOptions
  { accessToken :: Text
  , nick :: Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON GroupDMAddRecipientOptions

instance CalamityToJSON' GroupDMAddRecipientOptions where
  toPairs GroupDMAddRecipientOptions {..} =
    [ "access_token" .= accessToken
    , "nick" .= nick
    ]

$(makeFieldLabelsNoPrefix ''CreateMessageAttachment)
$(makeFieldLabelsNoPrefix ''CreateMessageOptions)
$(makeFieldLabelsNoPrefix ''CreateMessageAttachmentJson)
$(makeFieldLabelsNoPrefix ''AllowedMentions)
$(makeFieldLabelsNoPrefix ''ChannelUpdate)
$(makeFieldLabelsNoPrefix ''ChannelMessagesFilter)
$(makeFieldLabelsNoPrefix ''ChannelMessagesLimit)
$(makeFieldLabelsNoPrefix ''GetReactionsOptions)
$(makeFieldLabelsNoPrefix ''CreateChannelInviteOptions)
$(makeFieldLabelsNoPrefix ''GroupDMAddRecipientOptions)

data ChannelRequest a where
  CreateMessage :: (HasID Channel c) => c -> CreateMessageOptions -> ChannelRequest Message
  CrosspostMessage :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest Message
  GetMessage :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest Message
  EditMessage :: (HasID Channel c, HasID Message m) => c -> m -> EditMessageData -> ChannelRequest Message
  DeleteMessage :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  BulkDeleteMessages :: (HasID Channel c, HasID Message m) => c -> [m] -> ChannelRequest ()
  GetChannel :: (HasID Channel c) => c -> ChannelRequest Channel
  ModifyChannel :: (HasID Channel c) => c -> ChannelUpdate -> ChannelRequest Channel
  DeleteChannel :: (HasID Channel c) => c -> ChannelRequest ()
  GetChannelMessages :: (HasID Channel c) => c -> Maybe ChannelMessagesFilter -> Maybe ChannelMessagesLimit -> ChannelRequest [Message]
  CreateReaction :: (HasID Channel c, HasID Message m) => c -> m -> RawEmoji -> ChannelRequest ()
  DeleteOwnReaction :: (HasID Channel c, HasID Message m) => c -> m -> RawEmoji -> ChannelRequest ()
  DeleteUserReaction :: (HasID Channel c, HasID Message m, HasID User u) => c -> m -> RawEmoji -> u -> ChannelRequest ()
  GetReactions :: (HasID Channel c, HasID Message m) => c -> m -> RawEmoji -> GetReactionsOptions -> ChannelRequest [User]
  DeleteAllReactions :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  GetChannelInvites :: (HasID Channel c) => c -> ChannelRequest [Invite]
  CreateChannelInvite :: (HasID Channel c) => c -> CreateChannelInviteOptions -> ChannelRequest Invite
  EditChannelPermissions :: (HasID Channel c) => c -> Overwrite -> ChannelRequest ()
  DeleteChannelPermission :: (HasID Channel c, HasID Overwrite o) => c -> o -> ChannelRequest ()
  TriggerTyping :: (HasID Channel c) => c -> ChannelRequest ()
  GetPinnedMessages :: (HasID Channel c) => c -> ChannelRequest [Message]
  AddPinnedMessage :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  DeletePinnedMessage :: (HasID Channel c, HasID Message m) => c -> m -> ChannelRequest ()
  GroupDMAddRecipient :: (HasID Channel c, HasID User u) => c -> u -> GroupDMAddRecipientOptions -> ChannelRequest ()
  GroupDMRemoveRecipient :: (HasID Channel c, HasID User u) => c -> u -> ChannelRequest ()

baseRoute :: Snowflake Channel -> RouteBuilder _
baseRoute id =
  mkRouteBuilder // S "channels" // ID @Channel
    & giveID id

renderEmoji :: RawEmoji -> Text
renderEmoji (UnicodeEmoji e) = e
renderEmoji (CustomEmoji e) = e ^. #name <> ":" <> showt (e ^. #id)

instance Request (ChannelRequest a) where
  type Result (ChannelRequest a) = a

  route (CreateMessage (getID -> id) _) =
    baseRoute id // S "messages"
      & buildRoute
  route (CrosspostMessage (getID -> id) (getID @Message -> mid)) =
    baseRoute id // S "messages" // ID @Message
      & giveID mid
      & buildRoute
  route (GetChannel (getID -> id)) =
    baseRoute id
      & buildRoute
  route (ModifyChannel (getID -> id) _) =
    baseRoute id
      & buildRoute
  route (DeleteChannel (getID -> id)) =
    baseRoute id
      & buildRoute
  route (GetChannelMessages (getID -> id) _ _) =
    baseRoute id // S "messages"
      & buildRoute
  route (GetMessage (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "messages" // ID @Message
      & giveID mid
      & buildRoute
  route (CreateReaction (getID -> cid) (getID @Message -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // PS @"emoji" // S "@me"
      & giveID mid
      & giveParam @"emoji" (renderEmoji emoji)
      & buildRoute
  route (DeleteOwnReaction (getID -> cid) (getID @Message -> mid) emoji) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // PS @"emoji" // S "@me"
      & giveID mid
      & giveParam @"emoji" (renderEmoji emoji)
      & buildRoute
  route (DeleteUserReaction (getID -> cid) (getID @Message -> mid) emoji (getID @User -> uid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // PS @"emoji" // ID @User
      & giveID mid
      & giveID uid
      & giveParam @"emoji" (renderEmoji emoji)
      & buildRoute
  route (GetReactions (getID -> cid) (getID @Message -> mid) emoji _) =
    baseRoute cid // S "messages" // ID @Message // S "reactions" // PS @"emoji"
      & giveID mid
      & giveParam @"emoji" (renderEmoji emoji)
      & buildRoute
  route (DeleteAllReactions (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "messages" // ID @Message // S "reactions"
      & giveID mid
      & buildRoute
  route (EditMessage (getID -> cid) (getID @Message -> mid) _) =
    baseRoute cid // S "messages" // ID @Message
      & giveID mid
      & buildRoute
  route (DeleteMessage (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "messages" // ID @Message
      & giveID mid
      & buildRoute
  route (BulkDeleteMessages (getID -> cid) _) =
    baseRoute cid // S "messages" // S "bulk-delete"
      & buildRoute
  route (GetChannelInvites (getID -> cid)) =
    baseRoute cid // S "invites"
      & buildRoute
  route (CreateChannelInvite (getID -> cid) _) =
    baseRoute cid // S "invites"
      & buildRoute
  route (EditChannelPermissions (getID -> cid) (getID @Overwrite -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
      & giveID oid
      & buildRoute
  route (DeleteChannelPermission (getID -> cid) (getID @Overwrite -> oid)) =
    baseRoute cid // S "permissions" // ID @Overwrite
      & giveID oid
      & buildRoute
  route (TriggerTyping (getID -> cid)) =
    baseRoute cid // S "typing"
      & buildRoute
  route (GetPinnedMessages (getID -> cid)) =
    baseRoute cid // S "pins"
      & buildRoute
  route (AddPinnedMessage (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "pins" // ID @Message
      & giveID mid
      & buildRoute
  route (DeletePinnedMessage (getID -> cid) (getID @Message -> mid)) =
    baseRoute cid // S "pins" // ID @Message
      & giveID mid
      & buildRoute
  route (GroupDMAddRecipient (getID -> cid) (getID @User -> uid) _) =
    baseRoute cid // S "recipients" // ID @User
      & giveID uid
      & buildRoute
  route (GroupDMRemoveRecipient (getID -> cid) (getID @User -> uid)) =
    baseRoute cid // S "recipients" // ID @User
      & giveID uid
      & buildRoute
  action (CreateMessage _ cm) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO (T.pack $ "files[" <> show n <> "]") content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (fromMaybe [] $ cm ^. #attachments) [(0 :: Int) ..]
        attachments = (\a -> zipWith attachmentPart a [0 ..]) <$> cm ^. #attachments
        jsonBody =
          CreateMessageJson
            { content = cm ^. #content
            , nonce = cm ^. #nonce
            , tts = cm ^. #tts
            , embeds = cm ^. #embeds
            , allowedMentions = cm ^. #allowedMentions
            , messageReference = cm ^. #messageReference
            , components = cm ^. #components
            , attachments = attachments
            }
    body <- reqBodyMultipart (partLBS "payload_json" (Aeson.encode jsonBody) : files)
    postWith' body u o
  action (CrosspostMessage _ _) = postEmpty
  action (GetChannel _) = getWith
  action (ModifyChannel _ p) = patchWith' (ReqBodyJson p)
  action (DeleteChannel _) = deleteWith
  action (GetChannelMessages _ (Just (ChannelMessagesAround (fromSnowflake -> a))) l) =
    getWithP ("around" =: a <> "limit" =:? (l ^? _Just % #limit))
  action (GetChannelMessages _ (Just (ChannelMessagesBefore (fromSnowflake -> a))) l) =
    getWithP ("before" =: a <> "limit" =:? (l ^? _Just % #limit))
  action (GetChannelMessages _ (Just (ChannelMessagesAfter (fromSnowflake -> a))) l) =
    getWithP ("after" =: a <> "limit" =:? (l ^? _Just % #limit))
  action (GetChannelMessages _ Nothing l) = getWithP ("limit" =:? (l ^? _Just % #limit))
  action (GetMessage _ _) = getWith
  action CreateReaction {} = putEmpty
  action DeleteOwnReaction {} = deleteWith
  action DeleteUserReaction {} = deleteWith
  action (GetReactions _ _ _ GetReactionsOptions {before, after, limit}) =
    getWithP
      ( "before" =:? (fromSnowflake <$> before)
          <> "after" =:? (fromSnowflake <$> after)
          <> "limit" =:? limit
      )
  action (DeleteAllReactions _ _) = deleteWith
  action (EditMessage _ _ o) = patchWith' (ReqBodyJson o)
  action (DeleteMessage _ _) = deleteWith
  action (BulkDeleteMessages _ (map (getID @Message) -> ids)) = postWith' (ReqBodyJson $ Aeson.object ["messages" Aeson..= ids])
  action (GetChannelInvites _) = getWith
  action (CreateChannelInvite _ o) = postWith' (ReqBodyJson o)
  action (EditChannelPermissions _ o) = putWith' (ReqBodyJson o)
  action (DeleteChannelPermission _ _) = deleteWith
  action (TriggerTyping _) = postEmpty
  action (GetPinnedMessages _) = getWith
  action (AddPinnedMessage _ _) = putEmpty
  action (DeletePinnedMessage _ _) = deleteWith
  action (GroupDMAddRecipient _ _ o) = putWith' (ReqBodyJson o)
  action (GroupDMRemoveRecipient _ _) = deleteWith
