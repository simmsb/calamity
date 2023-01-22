{-# LANGUAGE TemplateHaskell #-}

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
import Calamity.Commands.Dsl
import Calamity.Commands.Types
import Calamity.Metrics.Eff
import Calamity.Types.Model.Channel
import Calamity.Types.Model.Guild.Member (Member)
import Calamity.Types.Model.User (User)
import CalamityCommands.CommandUtils
import CalamityCommands.Context qualified as CC
import CalamityCommands.Error qualified as CC
import CalamityCommands.ParsePrefix qualified as CC
import CalamityCommands.Utils qualified as CC
import Control.Monad
import Data.Text qualified as T
import Data.Typeable
import Optics.TH (makeFieldLabelsNoPrefix)
import Polysemy qualified as P

data CmdInvokeFailReason c
  = NoContext
  | NotFound [T.Text]
  | CommandInvokeError c CC.CommandError

data CtxCommandError c = CtxCommandError
  { ctx :: c
  , err :: CC.CommandError
  }
  deriving (Show)

data CommandNotFound = CommandNotFound
  { msg :: Message
  , user :: User
  , member :: Maybe Member
  , path :: [T.Text]
  -- ^ The groups that were successfully parsed
  }
  deriving (Show)

newtype CommandInvoked c = CommandInvoked
  { ctx :: c
  }
  deriving stock (Show)

-- | A default interpretation for 'CC.ParsePrefix' that uses a single constant prefix.
useConstantPrefix :: T.Text -> P.Sem (CC.ParsePrefix Message ': r) a -> P.Sem r a
useConstantPrefix pre =
  P.interpret
    ( \case
        CC.ParsePrefix Message {content} -> pure ((pre,) <$> T.stripPrefix pre content)
    )

{- | Construct commands and groups from a command DSL, then registers an event
 handler on the bot that manages running those commands.


 Returns an action to remove the event handler, and the 'CommandHandler' that was constructed.

 ==== Command Resolution

 To determine if a command was invoked, and if so which command was invoked, the following happens:

     1. 'CalamityCommands.ParsePrefix.parsePrefix' is invoked, if no prefix is found: stop here.

     2. The input is read a word at a time until a matching command is found,
        fire the \"command-not-found\" event if not.

     3. A 'Calamity.Commands.Context.Context' is built, and the command invoked.

 ==== Custom Events

 This will fire the following events:

     1. 'CtxCommandError'

         Fired when a command returns an error.

     2. 'CommandNotFound'

         Fired when a valid prefix is used, but the command is not found.

     3. 'CommandInvoked'

         Fired when a command is successfully invoked.
-}
addCommands ::
  (BotC r, Typeable c, CommandContext c, P.Members [CC.ParsePrefix Message, CC.ConstructContext (Message, User, Maybe Member) c IO ()] r) =>
  P.Sem (DSLState c r) a ->
  P.Sem r (P.Sem r (), CommandHandler c, a)
addCommands m = do
  (handler, res) <- CC.buildCommands m
  remove <- react @'MessageCreateEvt $ \case
    (msg, Just user, member) -> do
      CC.parsePrefix msg >>= \case
        Just (prefix, cmd) -> do
          r <- CC.handleCommands handler (msg, user, member) prefix cmd
          case r of
            Left (CC.CommandInvokeError ctx e) -> fire . customEvt $ CtxCommandError ctx e
            Left (CC.NotFound path) -> fire . customEvt $ CommandNotFound msg user member path
            Left CC.NoContext -> pure () -- ignore if context couldn't be built
            Right (ctx, ()) -> do
              cmdInvoke <- registerCounter "commands_invoked" [("name", T.unwords $ commandPath (CC.ctxCommand ctx))]
              void $ addCounter 1 cmdInvoke
              fire . customEvt $ CommandInvoked ctx
        Nothing -> pure ()
    _ -> pure ()
  pure (remove, handler, res)

$(makeFieldLabelsNoPrefix ''CmdInvokeFailReason)
$(makeFieldLabelsNoPrefix ''CtxCommandError)
$(makeFieldLabelsNoPrefix ''CommandNotFound)
$(makeFieldLabelsNoPrefix ''CommandInvoked)
