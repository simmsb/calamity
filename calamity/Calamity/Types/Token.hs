-- | Discord tokens
module Calamity.Types.Token
    ( Token(..)
    , formatToken
    , rawToken ) where

import           Data.Text

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic as TSG

data Token
  = BotToken Text
  | UserToken Text
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric Token

formatToken :: Token -> Text
formatToken (BotToken t) = "Bot " <> t
formatToken (UserToken t) = t

rawToken :: Token -> Text
rawToken (BotToken t) = t
rawToken (UserToken t) = t
