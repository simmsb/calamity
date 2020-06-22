{-# LANGUAGE RecursiveDo #-}
-- | Command handler utilities
module Calamity.Commands.Utils
    ( addCommands
    , buildCommands
    , buildContext
    , handleCommands
    , findCommand ) where

import           Calamity.Cache.Eff
import           Calamity.Metrics.Eff
import           Calamity.Client.Client
import           Calamity.Client.Types
import           Calamity.Commands.AliasType
import           Calamity.Commands.Command
import           Calamity.Commands.CommandUtils
import           Calamity.Commands.Context
import           Calamity.Commands.Dsl
import           Calamity.Commands.Handler
import           Calamity.Commands.Error
import           Calamity.Commands.Group
import           Calamity.Commands.ParsePrefix
import           Calamity.Internal.LocalWriter
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Lens                   hiding ( Context )
import           Control.Monad

import           Data.Char                      ( isSpace )
import qualified Data.HashMap.Lazy              as LH
import qualified Data.Text                      as S
import qualified Data.Text.Lazy                 as L

import qualified Polysemy                       as P
import qualified Polysemy.Error                 as P
import qualified Polysemy.Fail                  as P
import qualified Polysemy.Tagged                as P
import qualified Polysemy.Fixpoint              as P
import qualified Polysemy.Reader                as P

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

data FailReason
  = NoCtx
  | NF [L.Text]
  | ERR Context CommandError

-- | Construct commands and groups from a command DSL, then registers an event
-- handler on the bot that manages running those commands.
--
-- This registers the following event handler for: @"invoke-command" ('Message',
-- 'L.Text')@ that takes a message and the command string to invoke (without a
-- prefix)
--
-- Returns an action to remove the event handler, and the 'CommandHandler' that was constructed
addCommands :: (BotC r, P.Member ParsePrefix r) => P.Sem (DSLState r) a -> P.Sem r (P.Sem r (), CommandHandler, a)
addCommands m = do
  (handler, res) <- buildCommands m
  remove <- react @'MessageCreateEvt $ \msg -> do
    parsePrefix msg >>= \case
      Just (prefix, cmd) ->
        handleCommands handler msg prefix cmd
      Nothing -> pure ()
  remove' <- react @('CustomEvt "invoke-command" (Message, L.Text)) $ \(msg, cmd) -> do
    handleCommands handler msg "" cmd
  pure (remove *> remove', handler, res)

-- | Manages parsing messages and handling commands for a CommandHandler.
--
-- ==== Custom Events
--
-- This will fire the following events:
--
--     1. @"command-error" ('Context', 'CommandError')@
--
--         Fired when a command returns an error.
--
--     2. @"command-not-found" ['Data.Text.Lazy.Text']@
--
--         Fired when a valid prefix is used, but the command is not found.
--
--     3. @"command-invoked" 'Context'@
--
--         Fired when a command is successfully invoked.
--
handleCommands :: (BotC r, P.Member ParsePrefix r)
               => CommandHandler
               -> Message -- ^ The message that invoked the command
               -> L.Text -- ^ The prefix used
               -> L.Text -- ^ The command string, without a prefix
               -> P.Sem r ()
handleCommands handler msg prefix cmd = do
  err <- P.runError $ do
    (command, unparsedParams) <- P.fromEither $ mapLeft NF $ findCommand handler cmd
    ctx <- P.note NoCtx =<< buildContext msg prefix command unparsedParams
    P.fromEither . mapLeft (ERR ctx) =<< invokeCommand ctx (ctx ^. #command)
    pure ctx
  case err of
    Left (ERR ctx e) -> fire $ customEvt @"command-error" (ctx, e)
    Left (NF path)   -> fire $ customEvt @"command-not-found" path
    Left _           -> pure () -- ignore if no prefix or if context couldn't be built
    Right ctx        -> do
      cmdInvoke <- registerCounter "commands_invoked" [("name", S.unwords $ commandPath (ctx ^. #command))]
      void $ addCounter 1 cmdInvoke
      fire $ customEvt @"command-invoked" ctx


-- | Run a command DSL, returning the constructed 'CommandHandler'
buildCommands :: forall r a. P.Member (P.Final IO) r
              => P.Sem (DSLState r) a
              -> P.Sem r (CommandHandler, a)
buildCommands m = P.fixpointToFinal $ mdo
  (groups, (cmds, a)) <- inner handler m
  let handler = CommandHandler groups cmds
  pure (handler, a)

  where inner :: CommandHandler -> P.Sem (DSLState r) a
              -> P.Sem (P.Fixpoint ': r) (LH.HashMap S.Text (Group, AliasType),
                                          (LH.HashMap S.Text (Command, AliasType), a))
        inner h =
          P.runReader h .
          P.runReader [] .
          P.runReader defaultHelp . P.untag @"original-help" .
          P.runReader defaultHelp .
          P.runReader Nothing .
          runLocalWriter @(LH.HashMap S.Text (Group, AliasType)) .
          runLocalWriter @(LH.HashMap S.Text (Command, AliasType))
        defaultHelp = (const "This command or group has no help.")


-- | Attempt to build the context for a command
buildContext :: BotC r => Message -> L.Text -> Command -> L.Text -> P.Sem r (Maybe Context)
buildContext msg prefix command unparsed = (rightToMaybe <$>) . P.runFail $ do
  guild <- join <$> getGuild `traverse` (msg ^. #guildID)
  let member = guild ^? _Just . #members . ix (coerceSnowflake $ getID @User msg)
  let gchan = guild ^? _Just . #channels . ix (coerceSnowflake $ getID @Channel msg)
  Just channel <- case gchan of
    Just chan -> pure . pure $ GuildChannel' chan
    _         -> DMChannel' <<$>> getDM (coerceSnowflake $ getID @Channel msg)
  Just user <- getUser $ getID msg

  pure $ Context msg guild member channel user command prefix unparsed

nextWord :: L.Text -> (L.Text, L.Text)
nextWord = L.break isSpace . L.stripStart

-- | Attempt to find what command was used.
--
-- On error: returns the path of existing groups that were found, so @"group0
-- group1 group2 notacommand"@ will error with @Left ["group0", "group1",
-- "group2"]@
--
-- On success: returns the command that was invoked, and the remaining text
-- after it.
--
-- This function isn't greedy, if you have a group and a command at the same
-- level, this will find the command first and ignore the group.
findCommand :: CommandHandler -> L.Text -> Either [L.Text] (Command, L.Text)
findCommand handler msg = goH $ nextWord msg
  where
    goH :: (L.Text, L.Text) -> Either [L.Text] (Command, L.Text)
    goH ("", _) = Left []
    goH (x, xs) = attachSoFar x
      (((, xs) <$> attachInitial (LH.lookup (L.toStrict x) (handler ^. #commands)))
       <> (attachInitial (LH.lookup (L.toStrict x) (handler ^. #groups)) >>= goG (nextWord xs)))

    goG :: (L.Text, L.Text) -> Group -> Either [L.Text] (Command, L.Text)
    goG ("", _) _ = Left []
    goG (x, xs) g = attachSoFar x
      (((, xs) <$> attachInitial (LH.lookup (L.toStrict x) (g ^. #commands)))
       <> (attachInitial (LH.lookup (L.toStrict x) (g ^. #children)) >>= goG (nextWord xs)))

    attachInitial :: Maybe (a, b) -> Either [L.Text] a
    attachInitial (Just (a, _)) = Right a
    attachInitial Nothing = Left []

    attachSoFar :: L.Text -> Either [L.Text] a -> Either [L.Text] a
    attachSoFar cmd (Left xs) = Left (cmd:xs)
    attachSoFar _ r = r
