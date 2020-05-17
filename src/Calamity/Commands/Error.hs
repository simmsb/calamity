-- | Command errors
module Calamity.Commands.Error
    ( CommandError(..) ) where

import qualified Data.Text        as S
import qualified Data.Text.Lazy   as L

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic as TSG

data CommandError
  = ParseError S.Text -- ^ The type of the parser
               L.Text -- ^ The reason that parsing failed
               (L.Text, L.Text) -- ^ How much was parsed and how much was remaining
  | CheckError S.Text -- ^ The name of the check that failed
               L.Text -- ^ The reason for the check failing
  | InvokeError S.Text -- ^ The name of the command that failed
                L.Text -- ^ The reason for failing
  deriving ( Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric CommandError
