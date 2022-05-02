-- | Interaction related utilities
module Calamity.Interactions.Utils (
  respond,
  respondEphemeral,
  followUp,
  followUpEphemeral,
  edit,
  defer,
  deferEphemeral,
  deferComponent,
  pushModal,
  userLocalState,
) where

import Calamity.HTTP
import Calamity.Interactions.Eff (InteractionEff, getInteractionID, getInteractionToken, getInteractionUser, getApplicationID)
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import Calamity.Types.Model.Channel.Component (Component (ActionRow'))
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake (Snowflake)
import Calamity.Types.Tellable
import Optics
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Polysemy as P
import qualified Polysemy.State as P
import System.Random (getStdRandom, uniform)

{- | Provide local state semantics to a running view, the state is keyed to the
 user invoking the interaction
-}
userLocalState ::
  forall r s a.
  P.Member InteractionEff r =>
  -- | Initial state
  s ->
  P.Sem (P.State s ': r) a ->
  P.Sem r a
userLocalState s =
  P.evalState H.empty
    . P.reinterpret @(P.State s) @(P.State (H.HashMap (Snowflake User) s))
      ( \case
          P.Get -> do
            uid <- getInteractionUser
            P.gets (H.lookupDefault s uid)
          P.Put s -> do
            uid <- getInteractionUser
            P.modify' (H.insert uid s)
      )

-- | Respond to an interaction with a globally visible message
respond :: forall t r.
  (P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r, ToMessage t) =>
  t ->
  P.Sem r (Either RestError ())
respond (runToMessage -> msg) =
  let opts =
        InteractionCallbackMessageOptions
          { tts = msg ^. #tts
          , content = msg ^. #content
          , embeds = msg ^. #embeds
          , allowedMentions = msg ^. #allowedMentions
          , ephemeral = Just False
          , suppressEmbeds = Just False
          , components = msg ^. #components
          , attachments = msg ^. #attachments
          }
   in do
        interactionID <- getInteractionID
        interactionToken <- getInteractionToken
        invoke $ CreateResponseMessage interactionID interactionToken opts

-- | Respond to an interaction with an ephemeral message
respondEphemeral :: forall t r.
  (P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r, ToMessage t) =>
  t ->
  P.Sem r (Either RestError ())
respondEphemeral (runToMessage -> msg) =
  let opts =
        InteractionCallbackMessageOptions
          { tts = msg ^. #tts
          , content = msg ^. #content
          , embeds = msg ^. #embeds
          , allowedMentions = msg ^. #allowedMentions
          , ephemeral = Just True
          , suppressEmbeds = Just False
          , components = msg ^. #components
          , attachments = msg ^. #attachments
          }
   in do
        interactionID <- getInteractionID
        interactionToken <- getInteractionToken
        invoke $ CreateResponseMessage interactionID interactionToken opts

-- | Respond to an interaction by editing the message that triggered the interaction
edit :: forall t r.
  (P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r, ToMessage t) =>
  t ->
  P.Sem r (Either RestError ())
edit (runToMessage -> msg) =
  let opts =
        InteractionCallbackMessageOptions
          { tts = msg ^. #tts
          , content = msg ^. #content
          , embeds = msg ^. #embeds
          , allowedMentions = msg ^. #allowedMentions
          , ephemeral = Nothing
          , suppressEmbeds = Nothing
          , components = msg ^. #components
          , attachments = msg ^. #attachments
          }
   in do
        interactionID <- getInteractionID
        interactionToken <- getInteractionToken
        invoke $ CreateResponseUpdate interactionID interactionToken opts

-- | Create a follow up response to an interaction
followUp :: forall t r.
  (P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r, ToMessage t) =>
  t ->
  P.Sem r (Either RestError ())
followUp (runToMessage -> msg) =
  let opts =
        InteractionCallbackMessageOptions
          { tts = msg ^. #tts
          , content = msg ^. #content
          , embeds = msg ^. #embeds
          , allowedMentions = msg ^. #allowedMentions
          , ephemeral = Nothing
          , suppressEmbeds = Nothing
          , components = msg ^. #components
          , attachments = msg ^. #attachments
          }
   in do
        applicationID <- getApplicationID
        interactionToken <- getInteractionToken
        invoke $ CreateFollowupMessage applicationID interactionToken opts

-- | Create an ephemeral follow up response to an interaction
followUpEphemeral :: forall t r.
  (P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r, ToMessage t) =>
  t ->
  P.Sem r (Either RestError ())
followUpEphemeral (runToMessage -> msg) =
  let opts =
        InteractionCallbackMessageOptions
          { tts = msg ^. #tts
          , content = msg ^. #content
          , embeds = msg ^. #embeds
          , allowedMentions = msg ^. #allowedMentions
          , ephemeral = Just True
          , suppressEmbeds = Nothing
          , components = msg ^. #components
          , attachments = msg ^. #attachments
          }
   in do
        applicationID <- getApplicationID
        interactionToken <- getInteractionToken
        invoke $ CreateFollowupMessage applicationID interactionToken opts

-- | Defer an interaction and show a loading state, use @followUp@ later on
defer :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => P.Sem r (Either RestError ())
defer = do
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke $ CreateResponseDefer interactionID interactionToken False

-- | Defer an interaction and show an ephemeral loading state, use @followUp@
-- later on
deferEphemeral :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => P.Sem r (Either RestError ())
deferEphemeral = do
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke $ CreateResponseDefer interactionID interactionToken True

-- | Defer operation
deferComponent :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => P.Sem r (Either RestError ())
deferComponent = do
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke $ CreateResponseDeferComponent interactionID interactionToken

fixupActionRow :: Component -> Component
fixupActionRow r@(ActionRow' _) = r
fixupActionRow x = ActionRow' [x]

{- | Push a modal as a response to an interaction

 You should probably use this with 'Calamity.Interaction.View.runView'
-}
pushModal :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => Text -> [Component] -> P.Sem r (Either RestError ())
pushModal title c = do
  -- we don't actually use the custom id of the modal. the custom ids of the
  -- sub-components are enough to disambiguate
  cid <- P.embed $ getStdRandom uniform
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke . CreateResponseModal interactionID interactionToken $ InteractionCallbackModal cid title (map fixupActionRow c)
