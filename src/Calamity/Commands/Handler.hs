-- | A command handler
module Calamity.Commands.Handler
    ( CommandHandler(..)
    , addCommands
    , buildCommands ) where

import           Calamity.Client.Client
import           Calamity.Client.Types
import           Calamity.Commands.Check
import           Calamity.Commands.Command
import           Calamity.Commands.Context
import           Calamity.Commands.Group
import           Calamity.Commands.LocalWriter
import           Calamity.Commands.ParsePrefix

import           Control.Lens                  hiding ( Context )
import           Control.Monad

import qualified Data.Text.Lazy                as L

import           GHC.Generics

import qualified Polysemy                      as P
import qualified Polysemy.Fixpoint             as P
import qualified Polysemy.Reader               as P

data CommandHandler = CommandHandler
  { groups   :: [Group]
    -- ^ Top level groups
  , commands :: [Command]
    -- ^ Top level commands
  }
  deriving ( Generic )

addCommands :: (BotC r, P.Member ParsePrefix r)
            => P.Sem (LocalWriter [Command] ':
                       LocalWriter [Group] ':
                       P.Reader (Maybe Group) ':
                       P.Reader (Context -> L.Text) ':
                       P.Reader [Check] ':
                       P.Fixpoint ':
                       r) a
            -> P.Sem r (P.Sem r (), a)
addCommands m = do
  (handler, res) <- buildCommands m
  remove <- react @'MessageCreateEvt $ \msg -> do
    ctx' <- buildContext handler msg
    case ctx' of
      Just ctx -> void $ invokeCommand ctx (ctx ^. #command)
      -- TODO fire error event on command failure
      Nothing  -> pure ()
  pure (remove, res)

buildCommands :: P.Member (P.Final IO) r
              => P.Sem (LocalWriter [Command] ':
                         LocalWriter [Group] ':
                         P.Reader (Maybe Group) ':
                         P.Reader (Context -> L.Text) ':
                         P.Reader [Check] ':
                         P.Fixpoint ':
                         r) a
              -> P.Sem r (CommandHandler, a)
buildCommands =
  ((\(groups, (cmds, a)) -> (CommandHandler groups cmds, a)) <$>) .
  P.fixpointToFinal .
  P.runReader [] .
  P.runReader (const "This command or group has no help.") .
  P.runReader Nothing .
  runLocalWriter @[Group] .
  runLocalWriter @[Command]
