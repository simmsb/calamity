-- | Command groups
module Calamity.Commands.Group
    ( Group(..) ) where

import           Calamity.Commands.Check
import {-# SOURCE #-} Calamity.Commands.Command
import {-# SOURCE #-} Calamity.Commands.Context

import           Control.Lens              hiding ( (<.>), Context )

import qualified Data.HashMap.Lazy         as LH
import qualified Data.Text                 as S
import qualified Data.Text.Lazy            as L

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic          as TSG

data Group = Group
  { name     :: S.Text
  , parent   :: Maybe Group
  , commands :: LH.HashMap S.Text Command
  , children :: LH.HashMap S.Text Group
  , help     :: Context -> L.Text
  , checks   :: [Check]
  }
  deriving ( Generic )

data GroupS = GroupS
  { name     :: S.Text
  , parent   :: Maybe S.Text
  , commands :: LH.HashMap S.Text Command
  , children :: LH.HashMap S.Text Group
  }
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric GroupS

instance Show Group where
  showsPrec d Group { name, parent, commands, children } = showsPrec d $ GroupS name (parent ^? _Just . #name) commands children

instance TextShow Group where
  showbPrec d Group { name, parent, commands, children } = showbPrec d $ GroupS name (parent ^? _Just . #name) commands children
