-- | Interaction endpoints
module Calamity.HTTP.Interaction (
  ) where

import Calamity.HTTP (AllowedMentions, CreateMessageAttachment)
import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Internal.AesonThings
import Calamity.Internal.IntColour
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild
import Calamity.Types.Model.Interaction
import Calamity.Types.Model.User
import Calamity.Types.Model.Voice
import Calamity.Types.Snowflake
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Aeson.KeyMap as K
import Data.Aeson.Lens
import Data.Colour (Colour)
import Data.Default.Class
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import TextShow
import qualified TextShow.Generic as TSG

data InteractionCallback = InteractionCallback
  { type_ :: InteractionCallbackType
  , data_ :: Maybe Value
  }

data InteractionCallbackMessageOptions = InteractionCallbackMessageOptions
  { tts :: Maybe Bool
  , content :: Maybe Text
  , embeds :: [Embed]
  , allowedMentions :: Maybe AllowedMentions
  , ephemeral :: Maybe Bool
  , suppressEmbeds :: Maybe Bool
  , components :: [Component]
  , attachments :: [CreateMessageAttachment]
  }
  deriving (Show, Generic, Default)

newtype InteractionCallbackAutocomplete = InteractionCallbackAutocomplete
  { choices :: [InteractionCallbackAutocompleteChoice]
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via CalamityJSON InteractionCallbackAutocomplete

data InteractionCallbackAutocompleteChoice = InteractionCallbackAutocompleteChoice
  { name :: Text
  , nameLocalizations :: H.HashMap Text Text
  , -- | Either text or numeric
    value :: Value
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via CalamityJSON InteractionCallbackAutocompleteChoice

data InteractionCallbackModal = InteractionCallbackModal
  { customID :: Text
  , title :: Text
  , components :: [Component]
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via CalamityJSON InteractionCallbackModal

data InteractionCallbackType
  = PongType
  | ChannelMessageWithSourceType
  | DeferredChannelMessageWithSourceType
  | DeferredUpdateMessageType
  | UpdateMessageType
  | ApplicationCommandAutocompleteResultType
  | ModalType
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric InteractionCallbackType

instance ToJSON InteractionCallbackType where
  toEncoding ty = toEncoding @Int $ case ty of
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
    InteractionCallbackMessageOptions ->
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

instance Request (InteractionRequest a) where
  type Result (InteractionRequest a) = a

  -- TODO

  route a = undefined

  action a = undefined
