{-# LANGUAGE RecursiveDo #-}
-- | Command handler utilities
module Calamity.Commands.Utils
    ( addCommands
    , buildCommands
    , buildContext ) where

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
  = NoPrefix
  | NoCtx
  | NF [L.Text]
  | ERR Context CommandError

-- | Construct commands and groups from a command DSL, then registers an event
-- handler on the bot that manages running those commands.
--
-- Returns an action to remove the event handler, and the 'CommandHandler' that was constructed
addCommands :: (BotC r, P.Member ParsePrefix r) => P.Sem (DSLState r) a -> P.Sem r (P.Sem r (), CommandHandler, a)
addCommands m = do
  (handler, res) <- buildCommands m
  remove <- react @'MessageCreateEvt $ \msg -> do
    err <- P.runError $ do
      (prefix, rest) <- P.note NoPrefix =<< parsePrefix msg
      (command, unparsedParams) <- P.fromEither $ mapLeft NF $ findCommand handler rest
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
  pure (remove, handler, res)


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
