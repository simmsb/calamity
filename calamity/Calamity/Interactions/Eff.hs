{-# LANGUAGE TemplateHaskell #-}

-- | Effect for working with an interaction
module Calamity.Interactions.Eff (
  InteractionEff (..),
  getInteraction,
  getInteractionID,
  getApplicationID,
  getInteractionToken,
  getInteractionUser,
) where

import Calamity.Types.Model.Interaction
import Calamity.Types.Model.User (User)
import Calamity.Types.Snowflake
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Optics ((%), (^.), (^?), _Just)
import Polysemy
import Polysemy qualified as P

data InteractionEff m a where
  GetInteraction :: InteractionEff m Interaction

makeSem ''InteractionEff

getInteractionID :: (P.Member InteractionEff r) => P.Sem r (Snowflake Interaction)
getInteractionID = (^. #id) <$> getInteraction

getApplicationID :: (P.Member InteractionEff r) => P.Sem r (Snowflake Application)
getApplicationID = (^. #applicationID) <$> getInteraction

getInteractionToken :: (P.Member InteractionEff r) => P.Sem r InteractionToken
getInteractionToken = (^. #token) <$> getInteraction

getInteractionUser :: (P.Member InteractionEff r) => P.Sem r (Snowflake User)
getInteractionUser = do
  int <- getInteraction
  let uid = int ^? #user % _Just % #id
      mid = int ^? #member % _Just % #id
  pure . fromJust $ uid <|> mid
