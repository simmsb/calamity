{-# LANGUAGE TemplateHaskell #-}

-- | Effect for working with an interaction
module Calamity.Interactions.Eff (
  respond,
  respondEphemeral,
  edit,
  defer,
  deferEphemeral,
  deferComponent,
  InteractionEff (..),
  getInteraction,
  getInteractionID,
  getApplicationID,
  getInteractionToken,
) where

import Calamity.HTTP
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import Calamity.Types.Model.Interaction
import Calamity.Types.Snowflake
import Calamity.Types.Tellable
import Control.Lens ((^.))
import Polysemy
import qualified Polysemy as P

data InteractionEff m a where
  GetInteraction :: InteractionEff m Interaction

makeSem ''InteractionEff

getInteractionID :: P.Member InteractionEff r => P.Sem r (Snowflake Interaction)
getInteractionID = (^. #id) <$> getInteraction

getApplicationID :: P.Member InteractionEff r => P.Sem r (Snowflake Application)
getApplicationID = (^. #applicationID) <$> getInteraction

getInteractionToken :: P.Member InteractionEff r => P.Sem r InteractionToken
getInteractionToken = (^. #token) <$> getInteraction

respond ::
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

respondEphemeral ::
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

edit ::
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

defer :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => P.Sem r (Either RestError ())
defer = do
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke $ CreateResponseDefer interactionID interactionToken False

deferEphemeral :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => P.Sem r (Either RestError ())
deferEphemeral = do
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke $ CreateResponseDefer interactionID interactionToken True

deferComponent :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => P.Sem r (Either RestError ())
deferComponent = do
  interactionID <- getInteractionID
  interactionToken <- getInteractionToken
  invoke $ CreateResponseDeferComponent interactionID interactionToken

pushModal :: P.Members '[InteractionEff, RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r => () -> P.Sem r (Either RestError ())
pushModal = undefined -- TODO when we have views
