{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}

-- | Command handler utilities
module Calamity.Commands.Utils (
  addCommands,
  useConstantPrefix,
  CmdInvokeFailReason (..),
  CtxCommandError (..),
  CommandNotFound (..),
  CommandInvoked (..),
) where

import Calamity.Client.Client
import Calamity.Client.Types
import CalamityCommands.CommandUtils
import qualified CalamityCommands.Error as CC
import Calamity.Commands.Dsl
import Calamity.Commands.Types
import Calamity.Metrics.Eff
import Calamity.Types.Model.Channel
import qualified CalamityCommands.Utils as CC
import Control.Monad
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import GHC.Generics (Generic)
import qualified Polysemy as P
import qualified CalamityCommands.Context as CC
import qualified CalamityCommands.ParsePrefix as CC
import Data.Typeable

data CmdInvokeFailReason c
  = NoContext
  | NotFound [L.Text]
  | CommandInvokeError c CC.CommandError

data CtxCommandError c = CtxCommandError
  { ctx :: c
  , err :: CC.CommandError
  }
  deriving (Show, Generic)

data CommandNotFound = CommandNotFound
  { msg :: Message
  , -- | The groups that were successfully parsed
    path :: [L.Text]
  }
  deriving (Show, Generic)

newtype CommandInvoked c = CommandInvoked
  { ctx :: c
  }
  deriving stock (Show, Generic)

-- | A default interpretation for 'CC.ParsePrefix' that uses a single constant prefix.
useConstantPrefix :: L.Text -> P.Sem (CC.ParsePrefix Message ': r) a -> P.Sem r a
useConstantPrefix pre = P.interpret (\case
                                        CC.ParsePrefix Message { content } -> pure ((pre, ) <$> L.stripPrefix pre content))

-- | Construct commands and groups from a command DSL, then registers an event
-- handler on the bot that manages running those commands.
--
--
-- Returns an action to remove the event handler, and the 'CommandHandler' that was constructed.
--
-- ==== Command Resolution
--
-- To determine if a command was invoked, and if so which command was invoked, the following happens:
--
--     1. 'CalamityCommands.ParsePrefix.parsePrefix' is invoked, if no prefix is found: stop here.
--
--     2. The input is read a word at a time until a matching command is found,
--        fire the \"command-not-found\" event if not.
--
--     3. A 'Calamity.Commands.Context.Context' is built, and the command invoked.
--
-- ==== Custom Events
--
-- This will fire the following events:
--
--     1. 'CtxCommandError'
--
--         Fired when a command returns an error.
--
--     2. 'CommandNotFound'
--
--         Fired when a valid prefix is used, but the command is not found.
--
--     3. 'CommandInvoked'
--
--         Fired when a command is successfully invoked.
--
addCommands :: (BotC r, Typeable c, CommandContext c, P.Members [CC.ParsePrefix Message, CC.ConstructContext Message c IO ()] r)
  => P.Sem (DSLState c r) a -> P.Sem r (P.Sem r (), CommandHandler c, a)
addCommands m = do
  (handler, res) <- CC.buildCommands m
  remove <- react @'MessageCreateEvt $ \msg -> do
    CC.parsePrefix msg >>= \case
      Just (prefix, cmd) -> do
        r <- CC.handleCommands handler msg prefix cmd
        case r of
          Left (CC.CommandInvokeError ctx e) -> fire . customEvt $ CtxCommandError ctx e
          Left (CC.NotFound path)            -> fire . customEvt $ CommandNotFound msg path
          Left CC.NoContext                  -> pure () -- ignore if context couldn't be built
          Right (ctx, ())        -> do
            cmdInvoke <- registerCounter "commands_invoked" [("name", S.unwords $ commandPath (CC.ctxCommand ctx))]
            void $ addCounter 1 cmdInvoke
            fire . customEvt $ CommandInvoked ctx
      Nothing -> pure ()
  pure (remove, handler, res)
