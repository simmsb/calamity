-- | A command handler
module Calamity.Commands.Handler
    ( CommandHandler(..)
    , addCommands
    , buildCommands
    , buildContext ) where

import           Calamity.Cache.Eff
import           Calamity.Client.Client
import           Calamity.Client.Types
import           Calamity.Commands.Command
import           Calamity.Commands.CommandUtils
import           Calamity.Commands.Context
import           Calamity.Commands.Dsl
import           Calamity.Commands.Error
import           Calamity.Commands.Group
import           Calamity.Commands.LocalWriter
import           Calamity.Commands.ParsePrefix
import           Calamity.Internal.Utils
import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Control.Applicative
import           Control.Lens                   hiding ( Context )
import           Control.Monad

import qualified Data.HashMap.Lazy              as LH
import qualified Data.Text                      as S
import qualified Data.Text.Lazy                 as L

import           GHC.Generics

import qualified Polysemy                       as P
import qualified Polysemy.Error                 as P
import qualified Polysemy.Fail                  as P
import qualified Polysemy.Fixpoint              as P
import qualified Polysemy.Reader                as P

data CommandHandler = CommandHandler
  { groups   :: LH.HashMap S.Text Group
    -- ^ Top level groups
  , commands :: LH.HashMap S.Text Command
    -- ^ Top level commands
  }
  deriving ( Generic )

addCommands :: (BotC r, P.Member ParsePrefix r)
            => P.Sem (DSLState r) a
            -> P.Sem r (P.Sem r (), CommandHandler, a)
addCommands m = do
  (handler, res) <- buildCommands m
  remove <- react @'MessageCreateEvt $ \msg -> do
    err <- P.runError . P.runFail $ do
        Just (prefix, unparsedMessage) <- parsePrefix msg
        ctx <- P.note (NotFound . head . L.words $ unparsedMessage) =<< buildContext handler msg prefix unparsedMessage
        P.fromEither =<< invokeCommand ctx (ctx ^. #command)
        pure ctx
    case err of
      Left e -> fire $ customEvt @"command-error" e
      Right ctx -> fire $ customEvt @"command-run" ctx
  pure (remove, handler, res)

buildCommands :: P.Member (P.Final IO) r
              => P.Sem (DSLState r) a
              -> P.Sem r (CommandHandler, a)
buildCommands =
  ((\(groups, (cmds, a)) -> (CommandHandler groups cmds, a)) <$>) .
  P.fixpointToFinal .
  P.runReader [] .
  P.runReader (const "This command or group has no help.") .
  P.runReader Nothing .
  runLocalWriter @(LH.HashMap S.Text Group) .
  runLocalWriter @(LH.HashMap S.Text Command)

buildContext :: BotC r => CommandHandler -> Message -> L.Text -> L.Text -> P.Sem r (Maybe Context)
buildContext handler msg prefix unparsed = (rightToMaybe <$>) . P.runFail $ do
  Just command <- pure $ findCommand handler unparsed
  guild <- join <$> getGuild `traverse` (msg ^. #guildID)
  let member = guild ^? _Just . #members . ix (coerceSnowflake $ getID @User msg)
  let gchan = guild ^? _Just . #channels . ix (coerceSnowflake $ getID @Channel msg)
  Just channel <- case gchan of
    Just chan -> pure . pure $ GuildChannel' chan
    _         -> DMChannel' <<$>> getDM (coerceSnowflake $ getID @Channel msg)
  Just user <- getUser $ getID msg

  pure $ Context msg guild member channel user command prefix unparsed

findCommand :: CommandHandler -> L.Text -> Maybe Command
findCommand handler unparsedMessage = goH $ L.words unparsedMessage
  where
    goH :: [L.Text] -> Maybe Command
    goH (x:xs) = LH.lookup (L.toStrict x) (handler ^. #commands)
      <|> (LH.lookup (L.toStrict x) (handler ^. #groups) >>= goG xs)
    goH [] = Nothing

    goG :: [L.Text] -> Group -> Maybe Command
    goG (x:xs) g = LH.lookup (L.toStrict x) (g ^. #commands) <|> (LH.lookup (L.toStrict x) (g ^. #children) >>= goG xs)
    goG [] _ = Nothing
