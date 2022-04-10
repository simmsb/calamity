{-# LANGUAGE TemplateHaskell #-}

module Calamity.Types.TokenEff (
  TokenEff (..),
  getBotToken,
) where

import Calamity.Types.Token
import Polysemy

data TokenEff m a where
  GetBotToken :: TokenEff m Token

makeSem ''TokenEff
