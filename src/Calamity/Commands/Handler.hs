-- | A command handler
module Calamity.Commands.Handler
    ( CommandHandler(..) ) where

import           Calamity.Commands.Command
import           Calamity.Commands.Group

import           GHC.Generics

data CommandHandler = CommandHandler
  { groups   :: [Group]
    -- ^ Top level groups
  , commands :: [Command]
    -- ^ Top level commands
  }
  deriving ( Generic )
