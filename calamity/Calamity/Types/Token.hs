{-# LANGUAGE TemplateHaskell #-}

-- | Discord tokens
module Calamity.Types.Token (
  Token (..),
  formatToken,
  rawToken,
) where

import Data.Text (Text)
import Optics.TH
import TextShow.TH (deriveTextShow)

data Token
  = BotToken Text
  | UserToken Text
  deriving (Show)

formatToken :: Token -> Text
formatToken (BotToken t) = "Bot " <> t
formatToken (UserToken t) = t

rawToken :: Token -> Text
rawToken (BotToken t) = t
rawToken (UserToken t) = t

$(deriveTextShow ''Token)
$(makeFieldLabelsNoPrefix ''Token)
