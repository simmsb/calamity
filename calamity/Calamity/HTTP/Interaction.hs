{-# LANGUAGE TemplateHaskell #-}

-- | Interaction endpoints
module Calamity.HTTP.Interaction (
  InteractionRequest (..),
  InteractionCallbackMessageOptions (..),
  InteractionCallbackAutocomplete (..),
  InteractionCallbackAutocompleteChoice (..),
  InteractionCallbackModal (..),
) where

import Calamity.HTTP.Channel (AllowedMentions, CreateMessageAttachment (..))
import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import Calamity.Types.Model.Channel.Component (Component, CustomID)
import Calamity.Types.Model.Channel.Embed (Embed)
import Calamity.Types.Model.Channel.Message (Message)
import Calamity.Types.Model.Interaction
import Calamity.Types.Snowflake
import qualified Data.Aeson as Aeson
import Data.Bits (shiftL, (.|.))
import Data.Default.Class
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)
import Data.Monoid (First (First, getFirst))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Req
import Network.Mime
import Optics
import PyF

data InteractionCallback = InteractionCallback
  { type_ :: InteractionCallbackType
  , data_ :: Maybe Aeson.Value
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON InteractionCallback

instance CalamityToJSON' InteractionCallback where
  toPairs InteractionCallback {..} =
    [ "type" .= type_
    , "data" .?= data_
    ]

data InteractionCallbackMessageOptions = InteractionCallbackMessageOptions
  { tts :: Maybe Bool
  , content :: Maybe Text
  , embeds :: Maybe [Embed]
  , allowedMentions :: Maybe AllowedMentions
  , ephemeral :: Maybe Bool
  , suppressEmbeds :: Maybe Bool
  , components :: Maybe [Component]
  , attachments :: Maybe [CreateMessageAttachment]
  }
  deriving (Show)

instance Default InteractionCallbackMessageOptions where
  def = InteractionCallbackMessageOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data CreateMessageAttachmentJson = CreateMessageAttachmentJson
  { id :: Int
  , filename :: Text
  , description :: Maybe Text
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateMessageAttachmentJson

instance CalamityToJSON' CreateMessageAttachmentJson where
  toPairs CreateMessageAttachmentJson {..} =
    [ "id" .= id
    , "filename" .= filename
    , "description" .?= description
    ]

data CreateResponseMessageJson = CreateResponseMessageJson
  { tts :: Maybe Bool
  , content :: Maybe Text
  , embeds :: Maybe [Embed]
  , allowedMentions :: Maybe AllowedMentions
  , flags :: Maybe Int
  , components :: Maybe [Component]
  , attachments :: Maybe [CreateMessageAttachmentJson]
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON CreateResponseMessageJson

instance CalamityToJSON' CreateResponseMessageJson where
  toPairs CreateResponseMessageJson {..} =
    [ "tts" .?= tts
    , "content" .?= content
    , "embeds" .?= embeds
    , "allowed_mentions" .?= allowedMentions
    , "flags" .?= flags
    , "components" .?= components
    , "attachments" .?= attachments
    ]

newtype InteractionCallbackAutocomplete = InteractionCallbackAutocomplete
  { choices :: [InteractionCallbackAutocompleteChoice]
  }
  deriving stock (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON InteractionCallbackAutocomplete

instance CalamityToJSON' InteractionCallbackAutocomplete where
  toPairs InteractionCallbackAutocomplete {..} = ["choices" .= choices]

data InteractionCallbackAutocompleteChoice = InteractionCallbackAutocompleteChoice
  { name :: Text
  , nameLocalizations :: H.HashMap Text Text
  , -- | Either text or numeric
    value :: Aeson.Value
  }
  deriving stock (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON InteractionCallbackAutocompleteChoice

instance CalamityToJSON' InteractionCallbackAutocompleteChoice where
  toPairs InteractionCallbackAutocompleteChoice {..} =
    [ "name" .= name
    , "name_localizations" .= nameLocalizations
    , "value" .= value
    ]

data InteractionCallbackModal = InteractionCallbackModal
  { customID :: CustomID
  , title :: Text
  , components :: [Component]
  }
  deriving stock (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON InteractionCallbackModal

instance CalamityToJSON' InteractionCallbackModal where
  toPairs InteractionCallbackModal {..} =
    [ "custom_id" .= customID
    , "title" .= title
    , "components" .= components
    ]

data InteractionCallbackType
  = PongType
  | ChannelMessageWithSourceType
  | DeferredChannelMessageWithSourceType
  | DeferredUpdateMessageType
  | UpdateMessageType
  | ApplicationCommandAutocompleteResultType
  | ModalType
  deriving (Show)

instance Aeson.ToJSON InteractionCallbackType where
  toJSON ty = Aeson.toJSON @Int $ case ty of
    PongType -> 1
    ChannelMessageWithSourceType -> 4
    DeferredChannelMessageWithSourceType -> 5
    DeferredUpdateMessageType -> 6
    UpdateMessageType -> 7
    ApplicationCommandAutocompleteResultType -> 8
    ModalType -> 9
  toEncoding ty = Aeson.toEncoding @Int $ case ty of
    PongType -> 1
    ChannelMessageWithSourceType -> 4
    DeferredChannelMessageWithSourceType -> 5
    DeferredUpdateMessageType -> 6
    UpdateMessageType -> 7
    ApplicationCommandAutocompleteResultType -> 8
    ModalType -> 9

data InteractionRequest a where
  CreateResponseMessage ::
    (HasID Interaction i) =>
    i ->
    InteractionToken ->
    InteractionCallbackMessageOptions ->
    InteractionRequest ()
  -- | Ack an interaction and defer the response
  --
  -- This route triggers the 'thinking' message
  CreateResponseDefer ::
    (HasID Interaction i) =>
    i ->
    InteractionToken ->
    -- | Ephemeral
    Bool ->
    InteractionRequest ()
  -- | Ack an interaction and defer the response
  --
  -- This route is only usable by component interactions, and doesn't trigger a
  -- 'thinking' message
  CreateResponseDeferComponent ::
    (HasID Interaction i) =>
    i ->
    InteractionToken ->
    InteractionRequest ()
  CreateResponseUpdate ::
    (HasID Interaction i) =>
    i ->
    InteractionToken ->
    InteractionCallbackMessageOptions ->
    InteractionRequest ()
  CreateResponseAutocomplete ::
    (HasID Interaction i) =>
    i ->
    InteractionToken ->
    InteractionCallbackAutocomplete ->
    InteractionRequest ()
  CreateResponseModal ::
    (HasID Interaction i) =>
    i ->
    InteractionToken ->
    InteractionCallbackModal ->
    InteractionRequest ()
  GetOriginalInteractionResponse ::
    (HasID Application i) =>
    i ->
    InteractionToken ->
    InteractionRequest Message
  EditOriginalInteractionResponse ::
    (HasID Application i) =>
    i ->
    InteractionToken ->
    InteractionCallbackMessageOptions ->
    InteractionRequest Message
  DeleteOriginalInteractionResponse ::
    (HasID Application i) =>
    i ->
    InteractionToken ->
    InteractionRequest ()
  CreateFollowupMessage ::
    (HasID Application i) =>
    i ->
    InteractionToken ->
    InteractionCallbackMessageOptions ->
    InteractionRequest ()
  GetFollowupMessage ::
    (HasID Application i, HasID Message m) =>
    i ->
    m ->
    InteractionToken ->
    InteractionRequest Message
  EditFollowupMessage ::
    (HasID Application i, HasID Message m) =>
    i ->
    m ->
    InteractionToken ->
    InteractionCallbackMessageOptions ->
    InteractionRequest ()
  DeleteFollowupMessage ::
    (HasID Application i, HasID Message m) =>
    i ->
    m ->
    InteractionToken ->
    InteractionRequest ()

baseRoute :: Snowflake Application -> InteractionToken -> RouteBuilder _
baseRoute id (InteractionToken token) =
  mkRouteBuilder // S "webhooks" // ID @Application // S token
    & giveID id

foo :: Maybe a -> Maybe a -> (a -> a -> a) -> Maybe a
foo (Just x) (Just y) f = Just (f x y)
foo x y _ = getFirst $ First x <> First y

instance Request (InteractionRequest a) where
  type Result (InteractionRequest a) = a

  route (CreateResponseDefer (getID @Interaction -> iid) (InteractionToken token) _) =
    mkRouteBuilder // S "interactions" // ID @Interaction // S token // S "callback"
      & giveID iid
      & buildRoute
  route (CreateResponseDeferComponent (getID @Interaction -> iid) (InteractionToken token)) =
    mkRouteBuilder // S "interactions" // ID @Interaction // S token // S "callback"
      & giveID iid
      & buildRoute
  route (CreateResponseMessage (getID @Interaction -> iid) (InteractionToken token) _) =
    mkRouteBuilder // S "interactions" // ID @Interaction // S token // S "callback"
      & giveID iid
      & buildRoute
  route (CreateResponseUpdate (getID @Interaction -> iid) (InteractionToken token) _) =
    mkRouteBuilder // S "interactions" // ID @Interaction // S token // S "callback"
      & giveID iid
      & buildRoute
  route (CreateResponseAutocomplete (getID @Interaction -> iid) (InteractionToken token) _) =
    mkRouteBuilder // S "interactions" // ID @Interaction // S token // S "callback"
      & giveID iid
      & buildRoute
  route (CreateResponseModal (getID @Interaction -> iid) (InteractionToken token) _) =
    mkRouteBuilder // S "interactions" // ID @Interaction // S token // S "callback"
      & giveID iid
      & buildRoute
  route (GetOriginalInteractionResponse (getID @Application -> aid) token) =
    baseRoute aid token // S "messages" // S "@original" & buildRoute
  route (EditOriginalInteractionResponse (getID @Application -> aid) token _) =
    baseRoute aid token // S "messages" // S "@original" & buildRoute
  route (DeleteOriginalInteractionResponse (getID @Application -> aid) token) =
    baseRoute aid token // S "messages" // S "@original" & buildRoute
  route (CreateFollowupMessage (getID @Application -> aid) token _) =
    baseRoute aid token & buildRoute
  route (GetFollowupMessage (getID @Application -> aid) (getID @Message -> mid) token) =
    baseRoute aid token // S "messages" // ID @Message & giveID mid & buildRoute
  route (EditFollowupMessage (getID @Application -> aid) (getID @Message -> mid) token _) =
    baseRoute aid token // S "messages" // ID @Message & giveID mid & buildRoute
  route (DeleteFollowupMessage (getID @Application -> aid) (getID @Message -> mid) token) =
    baseRoute aid token // S "messages" // ID @Message & giveID mid & buildRoute

  action (CreateResponseDefer _ _ ephemeral) =
    let jsonBody =
          InteractionCallback
            { type_ = DeferredChannelMessageWithSourceType
            , data_ = if ephemeral then Just . Aeson.object $ [("flags", Aeson.Number 64)] else Nothing
            }
     in postWith' (ReqBodyJson jsonBody)
  action (CreateResponseDeferComponent _ _) =
    let jsonBody =
          InteractionCallback
            { type_ = DeferredUpdateMessageType
            , data_ = Nothing
            }
     in postWith' (ReqBodyJson jsonBody)
  action (CreateResponseMessage _ _ cm) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO [fmt|files[{n}]|] content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (fromMaybe [] $ cm ^. #attachments) [(0 :: Int) ..]
        attachments = (\a -> zipWith attachmentPart a [0 ..]) <$> cm ^. #attachments
        ephemeral = (\f -> if f then 1 `shiftL` 6 else 0) <$> cm ^. #ephemeral
        suppressEmbeds = (\f -> if f then 1 `shiftL` 2 else 0) <$> cm ^. #suppressEmbeds
        flags = foo ephemeral suppressEmbeds (.|.)
        jsonData =
          CreateResponseMessageJson
            { content = cm ^. #content
            , tts = cm ^. #tts
            , embeds = cm ^. #embeds
            , allowedMentions = cm ^. #allowedMentions
            , components = cm ^. #components
            , attachments = attachments
            , flags = flags
            }
        jsonBody =
          InteractionCallback
            { type_ = ChannelMessageWithSourceType
            , data_ = Just . Aeson.toJSON $ jsonData
            }
    body <- reqBodyMultipart (partLBS "payload_json" (Aeson.encode jsonBody) : files)
    postWith' body u o
  action (CreateResponseUpdate _ _ cm) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO [fmt|files[{n}]|] content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (fromMaybe [] $ cm ^. #attachments) [(0 :: Int) ..]
        attachments = (\a -> zipWith attachmentPart a [0 ..]) <$> cm ^. #attachments
        ephemeral = (\f -> if f then 1 `shiftL` 6 else 0) <$> cm ^. #ephemeral
        suppressEmbeds = (\f -> if f then 1 `shiftL` 2 else 0) <$> cm ^. #suppressEmbeds
        flags = foo ephemeral suppressEmbeds (.|.)
        jsonData =
          CreateResponseMessageJson
            { content = cm ^. #content
            , tts = cm ^. #tts
            , embeds = cm ^. #embeds
            , allowedMentions = cm ^. #allowedMentions
            , components = cm ^. #components
            , attachments = attachments
            , flags = flags
            }
        jsonBody =
          InteractionCallback
            { type_ = UpdateMessageType
            , data_ = Just . Aeson.toJSON $ jsonData
            }
    body <- reqBodyMultipart (partLBS "payload_json" (Aeson.encode jsonBody) : files)
    postWith' body u o
  action (CreateResponseAutocomplete _ _ ao) =
    let jsonBody =
          InteractionCallback
            { type_ = ApplicationCommandAutocompleteResultType
            , data_ = Just . Aeson.toJSON $ ao
            }
     in postWith' (ReqBodyJson jsonBody)
  action (CreateResponseModal _ _ mo) =
    let jsonBody =
          InteractionCallback
            { type_ = ModalType
            , data_ = Just . Aeson.toJSON $ mo
            }
     in postWith' (ReqBodyJson jsonBody)
  action (GetOriginalInteractionResponse _ _) = getWith
  action (EditOriginalInteractionResponse _ _ cm) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO [fmt|files[{n}]|] content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (fromMaybe [] $ cm ^. #attachments) [(0 :: Int) ..]
        attachments = (\a -> zipWith attachmentPart a [0 ..]) <$> cm ^. #attachments
        ephemeral = (\f -> if f then 1 `shiftL` 6 else 0) <$> cm ^. #ephemeral
        suppressEmbeds = (\f -> if f then 1 `shiftL` 2 else 0) <$> cm ^. #suppressEmbeds
        flags = foo ephemeral suppressEmbeds (.|.)
        jsonData =
          CreateResponseMessageJson
            { content = cm ^. #content
            , tts = cm ^. #tts
            , embeds = cm ^. #embeds
            , allowedMentions = cm ^. #allowedMentions
            , components = cm ^. #components
            , attachments = attachments
            , flags = flags
            }
        jsonBody =
          InteractionCallback
            { type_ = UpdateMessageType
            , data_ = Just . Aeson.toJSON $ jsonData
            }
    body <- reqBodyMultipart (partLBS "payload_json" (Aeson.encode jsonBody) : files)
    patchWith' body u o
  action (DeleteOriginalInteractionResponse _ _) = deleteWith
  action (CreateFollowupMessage _ _ cm) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO [fmt|files[{n}]|] content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (fromMaybe [] $ cm ^. #attachments) [(0 :: Int) ..]
        attachments = (\a -> zipWith attachmentPart a [0 ..]) <$> cm ^. #attachments
        ephemeral = (\f -> if f then 1 `shiftL` 6 else 0) <$> cm ^. #ephemeral
        suppressEmbeds = (\f -> if f then 1 `shiftL` 2 else 0) <$> cm ^. #suppressEmbeds
        flags = foo ephemeral suppressEmbeds (.|.)
        jsonData =
          CreateResponseMessageJson
            { content = cm ^. #content
            , tts = cm ^. #tts
            , embeds = cm ^. #embeds
            , allowedMentions = cm ^. #allowedMentions
            , components = cm ^. #components
            , attachments = attachments
            , flags = flags
            }
    body <- reqBodyMultipart (partLBS "payload_json" (Aeson.encode jsonData) : files)
    postWith' body u o
  action GetFollowupMessage {} = getWith
  action (EditFollowupMessage _ _ _ cm) = \u o -> do
    let filePart CreateMessageAttachment {filename, content} n =
          (partLBS @IO [fmt|files[{n}]|] content)
            { partFilename = Just (T.unpack filename)
            , partContentType = Just (defaultMimeLookup filename)
            }
        attachmentPart CreateMessageAttachment {filename, description} n =
          CreateMessageAttachmentJson n filename description
        files = zipWith filePart (fromMaybe [] $ cm ^. #attachments) [(0 :: Int) ..]
        attachments = (\a -> zipWith attachmentPart a [0 ..]) <$> cm ^. #attachments
        ephemeral = (\f -> if f then 1 `shiftL` 6 else 0) <$> cm ^. #ephemeral
        suppressEmbeds = (\f -> if f then 1 `shiftL` 2 else 0) <$> cm ^. #suppressEmbeds
        flags = foo ephemeral suppressEmbeds (.|.)
        jsonData =
          CreateResponseMessageJson
            { content = cm ^. #content
            , tts = cm ^. #tts
            , embeds = cm ^. #embeds
            , allowedMentions = cm ^. #allowedMentions
            , components = cm ^. #components
            , attachments = attachments
            , flags = flags
            }
    body <- reqBodyMultipart (partLBS "payload_json" (Aeson.encode jsonData) : files)
    patchWith' body u o
  action DeleteFollowupMessage {} = deleteWith

$(makeFieldLabelsNoPrefix ''InteractionCallbackMessageOptions)
$(makeFieldLabelsNoPrefix ''InteractionCallbackAutocomplete)
$(makeFieldLabelsNoPrefix ''InteractionCallbackAutocompleteChoice)
$(makeFieldLabelsNoPrefix ''InteractionCallbackModal)
