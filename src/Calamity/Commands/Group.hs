-- | Command groups
module Calamity.Commands.Group
    ( Group(..) ) where

import           Calamity.Commands.Check
import {-# SOURCE #-} Calamity.Commands.Command
import           Calamity.Commands.Context

import qualified Data.Text                 as S
import qualified Data.Text.Lazy            as L

import           GHC.Generics

data Group = Group
  { name     :: S.Text
  , parent   :: Maybe Group
  , commands :: [Command]
  , children :: [Group]
  , help     :: Context -> L.Text
  , checks   :: [Check]
  }
  deriving ( Generic )
