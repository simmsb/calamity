-- | Command groups
module Calamity.Commands.Group
    ( Group(..) ) where

import           Calamity.Commands.Check
import {-# SOURCE #-} Calamity.Commands.Command
import {-# SOURCE #-} Calamity.Commands.Context

import qualified Data.HashMap.Lazy         as LH
import qualified Data.Text                 as S
import qualified Data.Text.Lazy            as L

import           GHC.Generics

data Group = Group
  { name     :: S.Text
  , parent   :: Maybe Group
  , commands :: LH.HashMap S.Text Command
  , children :: LH.HashMap S.Text Group
  , help     :: Context -> L.Text
  , checks   :: [Check]
  }
  deriving ( Generic )
