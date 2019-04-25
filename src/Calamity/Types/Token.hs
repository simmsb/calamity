-- | Discord tokens
module Calamity.Types.Token
    ( Token(..)
    , formatToken
    , rawToken ) where

data Token
  = BotToken Text
  | UserToken Text
  deriving ( Generic, Show )

formatToken :: Token -> Text
formatToken (BotToken t) = "Bot " <> t
formatToken (UserToken t) = t

rawToken :: Token -> Text
rawToken (BotToken t) = t
rawToken (UserToken t) = t
