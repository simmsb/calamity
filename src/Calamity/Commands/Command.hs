-- | Commands and stuff
module Calamity.Commands.Command
    ( Command(..) ) where

import           Calamity.Commands.Check
import           Calamity.Commands.Context
import           Calamity.Commands.Error
import           Calamity.Commands.Group

import           Control.Lens              hiding ( (<.>), Context )

import           Data.Text                 as S
import           Data.Text.Lazy            as L

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic          as TSG

data Command = forall a. Command
  { name     :: S.Text
  , parent   :: Maybe Group
  , checks   :: [Check]
  , params   :: [S.Text]
  , help     :: Context -> L.Text
  , parser   :: Context -> IO (Either CommandError a)
  , callback :: (Context, a) -> IO (Maybe L.Text)
  }

data CommandS = CommandS
  { name   :: S.Text
  , params :: [S.Text]
  , parent :: Maybe S.Text
  , checks :: [S.Text]
  }
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric CommandS

instance Show Command where
  showsPrec d Command { name, params, parent, checks } = showsPrec d $ CommandS name params (parent ^? _Just . #name)
    (checks ^.. traverse . #name)

instance TextShow Command where
  showbPrec d Command { name, params, parent, checks } = showbPrec d $ CommandS name params (parent ^? _Just . #name)
    (checks ^.. traverse . #name)
