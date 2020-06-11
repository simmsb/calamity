-- | Command groups
module Calamity.Commands.Group
    ( Group(..) ) where

import           Calamity.Commands.AliasType
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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | A group of commands
data Group = Group
  { names    :: NonEmpty S.Text
  , parent   :: Maybe Group
  , commands :: LH.HashMap S.Text (Command, AliasType)
    -- ^ Any child commands of this group
  , children :: LH.HashMap S.Text (Group, AliasType)
    -- ^ Any child groups of this group
  , help     :: Context -> L.Text
    -- ^ A function producing the \'help' for the group
  , checks   :: [Check]
    -- -- ^ A list of checks that must pass
  }
  deriving ( Generic )

data GroupS = GroupS
  { names    :: NonEmpty S.Text
  , parent   :: Maybe S.Text
  , commands :: LH.HashMap S.Text (Command, AliasType)
  , children :: LH.HashMap S.Text (Group, AliasType)
  }
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric GroupS

instance Show Group where
  showsPrec d Group { names, parent, commands, children } = showsPrec d $ GroupS names (NE.head <$> parent ^? _Just . #names) commands children

instance TextShow Group where
  showbPrec d Group { names, parent, commands, children } = showbPrec d $ GroupS names (NE.head <$> parent ^? _Just . #names) commands children
