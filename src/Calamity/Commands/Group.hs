-- | Command groups
module Calamity.Commands.Group
    ( Group(..) ) where

import           Calamity.Commands.Check
import {-# SOURCE #-} Calamity.Commands.Command

import           Data.Text                 ( Text )

import           GHC.Generics

data Group = Group
  { name     :: Text
  , parent   :: Maybe Group
  , children :: [Command]
  , checks   :: [Check]
  }
  deriving ( Generic )
