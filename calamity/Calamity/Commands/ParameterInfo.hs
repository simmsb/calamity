-- | Parameter info for a command
module Calamity.Commands.ParameterInfo (
    ParameterInfo (..),
) where

import qualified Data.Text as S
import Data.Typeable

import GHC.Generics (Generic)
import TextShow
import qualified TextShow.Generic as TSG

data ParameterInfo = ParameterInfo
    { name :: Maybe S.Text
    , type_ :: TypeRep
    , typeDescription :: S.Text
    }
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric ParameterInfo
