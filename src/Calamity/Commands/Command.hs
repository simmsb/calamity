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

-- | A command
data Command = forall a. Command
  { name     :: S.Text
  , parent   :: Maybe Group
  , checks   :: [Check] -- TODO check checks on default help
    -- ^ A list of checks that must pass for this command to be invoked
  , params   :: [S.Text]
    -- ^ A list of the parameters the command takes, only used for constructing
    -- help messages.
  , help     :: Context -> L.Text
    -- ^ A function producing the \'help\' for the command.
  , parser   :: Context -> IO (Either CommandError a)
    -- ^ A function that parses the context for the command, producing the input
    -- @a@ for the command.
  , callback :: (Context, a) -> IO (Maybe L.Text)
    -- ^ A function that given the context and the input (@a@) of the command,
    -- performs the action of the command.
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
