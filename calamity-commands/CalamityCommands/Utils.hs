{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- | Command handler utilities
module CalamityCommands.Utils (
  buildCommands,
  processCommands,
  handleCommands,
  findCommand,
  CmdInvokeFailReason (..),
) where

import CalamityCommands.AliasType
import CalamityCommands.Command
import CalamityCommands.CommandUtils
import CalamityCommands.Context
import CalamityCommands.Dsl
import CalamityCommands.Error
import CalamityCommands.Group
import CalamityCommands.Handler
import CalamityCommands.Internal.LocalWriter
import CalamityCommands.ParsePrefix
import Control.Monad.Fix (MonadFix)
import Data.Char (isSpace)
import Data.HashMap.Lazy qualified as LH
import Data.Text qualified as T
import Optics
import Polysemy qualified as P
import Polysemy.Error qualified as P
import Polysemy.Fixpoint qualified as P
import Polysemy.Reader qualified as P
import Polysemy.Tagged qualified as P

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

data CmdInvokeFailReason c
  = NoContext
  | NotFound [T.Text]
  | CommandInvokeError c CommandError
  deriving (Show)

{- | Manages parsing messages and handling commands for a CommandHandler.

 Returns Nothing if the prefix didn't match.

 Returns Right with the context and result if the command succeeded in parsing
 and running, Left with the reason otherwise.
-}
processCommands ::
  ( Monad m
  , P.Members '[ParsePrefix msg, ConstructContext msg c m a, P.Embed m] r
  , CommandContext m c a
  ) =>
  CommandHandler m c a ->
  -- | The message that invoked the command
  msg ->
  P.Sem r (Maybe (Either (CmdInvokeFailReason c) (c, a)))
processCommands handler msg =
  parsePrefix msg >>= \case
    Just (pre, cmd) -> Just <$> handleCommands handler msg pre cmd
    Nothing -> pure Nothing

{- | Manages finding the invoked command and parsing parameters for a
   CommandHandler.

 Returns Right with the context and result if the command succeeded in parsing
 and running, Left with the reason otherwise.
-}
handleCommands ::
  ( Monad m
  , P.Members '[ConstructContext msg c m a, P.Embed m] r
  , CommandContext m c a
  ) =>
  CommandHandler m c a ->
  -- | The message that invoked the command
  msg ->
  -- | The prefix used
  T.Text ->
  -- | The command string, without a prefix
  T.Text ->
  P.Sem r (Either (CmdInvokeFailReason c) (c, a))
handleCommands handler msg prefix cmd = P.runError $ do
  (command, unparsedParams) <- P.fromEither . mapLeft NotFound $ findCommand handler cmd
  ctx <- P.note NoContext =<< constructContext (prefix, command, unparsedParams) msg
  r <- P.fromEither . mapLeft (CommandInvokeError ctx) =<< invokeCommand ctx (ctxCommand ctx)
  pure (ctx, r)

-- | Run a command DSL, returning the constructed 'CommandHandler'
buildCommands ::
  forall r c m a x.
  (Monad m, MonadFix m, P.Member (P.Final m) r) =>
  P.Sem (DSLState m c a r) x ->
  P.Sem r (CommandHandler m c a, x)
buildCommands m = P.fixpointToFinal $ mdo
  (groups, (cmds, a)) <- inner handler m
  let handler = CommandHandler groups cmds
  pure (handler, a)
  where
    inner ::
      CommandHandler m c a ->
      P.Sem (DSLState m c a r) x ->
      P.Sem
        (P.Fixpoint ': r)
        ( LH.HashMap T.Text (Group m c a, AliasType)
        , (LH.HashMap T.Text (Command m c a, AliasType), x)
        )
    inner h =
      P.runReader h
        . P.runReader []
        . P.runReader defaultHelp
        . P.untag @"original-help"
        . P.runReader defaultHelp
        . P.runReader False
        . P.untag @"hidden"
        . P.runReader Nothing
        . runLocalWriter @(LH.HashMap T.Text (Group m c a, AliasType))
        . runLocalWriter @(LH.HashMap T.Text (Command m c a, AliasType))
    defaultHelp = const "This command or group has no help."

nextWord :: T.Text -> (T.Text, T.Text)
nextWord = T.break isSpace . T.stripStart

{- | Attempt to find what command was used.

 On error: returns the path of existing groups that were found, so @"group0
 group1 group2 notacommand"@ will error with @Left ["group0", "group1",
 "group2"]@

 On success: returns the command that was invoked, and the remaining text
 after it.

 This function isn't greedy, if you have a group and a command at the same
 level, this will find the command first and ignore the group.
-}
findCommand :: forall c a m. CommandHandler m c a -> T.Text -> Either [T.Text] (Command m c a, T.Text)
findCommand handler msg = goH $ nextWord msg
  where
    goH :: (T.Text, T.Text) -> Either [T.Text] (Command m c a, T.Text)
    goH ("", _) = Left []
    goH (x, xs) =
      attachSoFar
        x
        ( ((,xs) <$> attachInitial (LH.lookup x (handler ^. #commands)))
            <> (attachInitial (LH.lookup x (handler ^. #groups)) >>= goG (nextWord xs))
        )

    goG :: (T.Text, T.Text) -> Group m c a -> Either [T.Text] (Command m c a, T.Text)
    goG ("", _) _ = Left []
    goG (x, xs) g =
      attachSoFar
        x
        ( ((,xs) <$> attachInitial (LH.lookup x (g ^. #commands)))
            <> (attachInitial (LH.lookup x (g ^. #children)) >>= goG (nextWord xs))
        )

    attachInitial :: forall a b. Maybe (a, b) -> Either [T.Text] a
    attachInitial (Just (a, _)) = Right a
    attachInitial Nothing = Left []

    attachSoFar :: forall a. T.Text -> Either [T.Text] a -> Either [T.Text] a
    attachSoFar cmd (Left xs) = Left (cmd : xs)
    attachSoFar _ r = r
