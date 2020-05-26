-- | Command utilities
module Calamity.Commands.CommandUtils
    ( TypedCommandC
    , CommandForParsers
    , buildCommand
    , buildCommand'
    , buildParser
    , buildCallback
    , runCommand
    , invokeCommand
    , groupPath
    , commandPath
    , commandParams ) where

import           Calamity.Commands.Check
import           Calamity.Commands.Command
import           Calamity.Commands.Context
import           Calamity.Commands.Error
import           Calamity.Commands.Group
import           Calamity.Commands.Parser
import           Calamity.Internal.RunIntoIO
import           Calamity.Internal.Utils

import           Control.Lens                hiding ( (<.>), Context )
import           Control.Monad

import           Data.Foldable
import           Data.Kind
import           Data.Maybe
import           Data.Text                   as S
import           Data.Text.Lazy              as L

import qualified Polysemy                    as P
import qualified Polysemy.Error              as P
import qualified Polysemy.Fail               as P

groupPath :: Group -> [S.Text]
groupPath grp = maybe [] groupPath (grp ^. #parent) ++ [grp ^. #name]

commandPath :: Command -> [S.Text]
commandPath Command { name, parent } = maybe [] groupPath parent ++ [name]

commandParams :: Command -> L.Text
commandParams Command { params } = L.fromStrict $ S.unwords params

-- | Given the properties of a 'Command' with the @parser@ and @callback@ in the
-- 'P.Sem' monad, build a command by transforming the Polysemy actions into IO
-- actions.
buildCommand' :: P.Member (P.Final IO) r
              => S.Text
              -> Maybe Group
              -> [Check]
              -> [S.Text]
              -> (Context -> L.Text)
              -> (Context -> P.Sem r (Either CommandError a))
              -> ((Context, a) -> P.Sem (P.Fail ': r) ())
              -> P.Sem r Command
buildCommand' name parent checks params help parser cb = do
  cb' <- buildCallback cb
  parser' <- buildParser name parser
  pure $ Command name parent checks params help parser' cb'

-- | Given the properties of a 'Command', a callback, and a type level list of
-- the parameters, build a command by constructing a parser and wiring it up to
-- the callback.
--
-- ==== Examples
--
-- Building a command that bans a user by id.
--
-- @
-- 'buildCommand' @\'['Named' "user" ('Snowflake' 'User'), 'Named' "reason" ('KleeneStarConcat' 'S.Text')]
--    "ban" 'Nothing' [] ('const' "Ban a user") $ \ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
--      'Just' guild -> do
--        'void' . 'Calamity.HTTP.invoke' . 'Calamity.HTTP.reason' r $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid
--        'void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
--      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
-- @
buildCommand :: forall ps r.
             (P.Member (P.Final IO) r, TypedCommandC ps r)
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> L.Text)
             -> (Context -> CommandForParsers ps r)
             -> P.Sem r Command
buildCommand name parent checks help command = let (parser, cb) = buildTypedCommand @ps command
                                               in buildCommand' name parent checks (paramNames @ps @r) help parser cb

-- | Given the name of the command the parser is for and a parser function in
-- the 'P.Sem' monad, build a parser by transforming the Polysemy action into an
-- IO action.
buildParser :: P.Member (P.Final IO) r
            => S.Text
            -> (Context -> P.Sem r (Either CommandError a))
            -> P.Sem r (Context -> IO (Either CommandError a))
buildParser name cb = do
  cb' <- bindSemToIO cb
  let cb'' ctx = fromMaybe (Left $ ParseError ("Parser for command: " <> name) "failed internally") <$> cb' ctx
  pure cb''

-- | Given a callback for a command in the 'P.Sem' monad, build a command callback by
-- transforming the Polysemy action into an IO action.
buildCallback
  :: P.Member (P.Final IO) r => ((Context, a) -> P.Sem (P.Fail ': r) ()) -> P.Sem r ((Context, a) -> IO (Maybe L.Text))
buildCallback cb = do
  cb' <- bindSemToIO (\x -> P.runFail (cb x) <&> \case
                        Left e -> Just $ L.pack e
                        _      -> Nothing)
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure cb''

-- | Given an invokation 'Context', run a command. This does not perform the command's checks.
runCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
runCommand ctx Command { name, parser, callback } = P.embed (parser ctx) >>= \case
  Left e   -> pure $ Left e
  Right p' -> P.embed (callback (ctx, p')) <&> justToEither . (InvokeError name <$>)

-- | Given an invokation 'Context', first run all of the command's checks, then
-- run the command if they all pass.
invokeCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
invokeCommand ctx cmd@Command { checks } = P.runError $ do
  for_ checks (P.fromEither <=< runCheck ctx)
  P.fromEither =<< runCommand ctx cmd

type CommandSemType r = P.Sem (P.Fail ': r) ()

-- | Some constraints used for making parameter typed commands work
type TypedCommandC ps r =
  ( ApplyTupRes (ParserResult (ListToTup ps)) (CommandSemType r) ~ CommandForParsers ps r
  , Parser (ListToTup ps) r
  , ApplyTup (ParserResult (ListToTup ps)) (CommandSemType r)
  , ParamNamesForParsers ps r
  )

buildTypedCommand
  :: forall (ps :: [Type]) a r.
  (TypedCommandC ps r, a ~ ParserResult (ListToTup ps))
  => (Context -> CommandForParsers ps r)
  -> ( Context
         -> P.Sem r (Either CommandError a)
     , (Context, a)
         -> P.Sem (P.Fail ': r) ())
buildTypedCommand cmd = let parser ctx = buildTypedCommandParser @ps ctx (ctx ^. #unparsedParams)
                            consumer (ctx, r) = applyTup (cmd ctx) r
                        in (parser, consumer)

class ParamNamesForParsers (ps :: [Type]) r where
  paramNames :: [S.Text]

instance ParamNamesForParsers '[] r where
  paramNames = []

instance (Parser x r, ParamNamesForParsers xs r) => ParamNamesForParsers (x : xs) r where
  paramNames = (parserName @x @r : paramNames @xs @r)

class ApplyTup a b where
  type ApplyTupRes a b

  applyTup :: ApplyTupRes a b -> a -> b

instance ApplyTup as b => ApplyTup (a, as) b where
  type ApplyTupRes (a, as) b = a -> ApplyTupRes as b

  applyTup f (a, as) = applyTup (f a) as

instance ApplyTup () b where
  type ApplyTupRes () b = b

  applyTup r () = r

buildTypedCommandParser :: forall (ps :: [Type]) r. Parser (ListToTup ps) r => Context -> L.Text -> P.Sem r (Either CommandError (ParserResult (ListToTup ps)))
buildTypedCommandParser ctx t = (runCommandParser ctx t $ parse @(ListToTup ps) @r) <&> \case
  Right r -> Right r
  Left (n, e)  -> Left $ ParseError n e

type family ListToTup (ps :: [Type]) where
  ListToTup '[] = ()
  ListToTup (x ': xs) = (x, ListToTup xs)

-- | Transform a type level list of types implementing the parser typeclass into
-- the type a command callback matching those parameters should be.
type family CommandForParsers (ps :: [Type]) r where
  CommandForParsers '[] r = P.Sem (P.Fail ': r) ()
  CommandForParsers (x ': xs) r = ParserResult x -> CommandForParsers xs r
