{-# LANGUAGE TemplateHaskell #-}

-- | Parameter info for a command
module CalamityCommands.ParameterInfo (
  ParameterInfo (..),
) where

import qualified Data.Text as S
import Data.Typeable
import Optics.TH (makeFieldLabelsNoPrefix)
import TextShow.TH (deriveTextShow)

data ParameterInfo = ParameterInfo
  { name :: Maybe S.Text
  , type_ :: TypeRep
  , typeDescription :: S.Text
  }
  deriving (Show)

$(deriveTextShow ''ParameterInfo)
$(makeFieldLabelsNoPrefix ''ParameterInfo)
