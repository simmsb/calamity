-- | Command utilities
module CalamityCommands.CommandUtils (
  TypedCommandC,
  CommandForParsers,
  buildCommand,
  buildCommand',
  buildParser,
  buildCallback,
  runCommand,
  invokeCommand,
  groupPath,
  commandPath,
  commandParams,
) where

import CalamityCommands.Check
import CalamityCommands.Command
import CalamityCommands.Context
import CalamityCommands.Error
import CalamityCommands.Group
import CalamityCommands.Internal.RunIntoM
import CalamityCommands.Internal.Utils
import CalamityCommands.ParameterInfo
import CalamityCommands.Parser

import Control.Lens hiding (Context, (<.>))
import Control.Monad

import Data.Foldable
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Fail as P

groupPath :: Group m c a -> [S.Text]
groupPath Group{names, parent} = foldMap groupPath parent <> [NE.head names]

commandPath :: Command m c a -> [S.Text]
commandPath Command{names, parent} = foldMap groupPath parent <> [NE.head names]

-- | Format a command's parameters
commandParams :: Command m c a -> L.Text
commandParams Command{params} =
  let formatted =
        map
          ( \(ParameterInfo (fromMaybe "" -> name) type_ _) ->
              "`" <> name <> ":" <> S.pack (show type_) <> "`"
          )
          params
   in L.fromStrict $ S.intercalate ", " formatted

{- | Given the properties of a 'Command' with the @parser@ and @callback@ in the
 'P.Sem' monad, build a command by transforming the Polysemy actions into @m@
 actions.
-}
buildCommand' ::
  forall c m a p r.
  (Monad m, P.Member (P.Final m) r) =>
  -- | The name (and aliases) of the command
  NonEmpty S.Text ->
  -- | The parent group of the command
  Maybe (Group m c a) ->
  -- | If the command is hidden
  Bool ->
  -- | The checks for the command
  [Check m c] ->
  -- | The command's parameter metadata
  [ParameterInfo] ->
  -- | The help generator for this command
  (c -> L.Text) ->
  -- | The parser for this command
  (c -> P.Sem r (Either CommandError p)) ->
  -- | The callback for this command
  ((c, p) -> P.Sem (P.Fail ': r) a) ->
  P.Sem r (Command m c a)
buildCommand' names@(name :| _) parent hidden checks params help parser cb = do
  cb' <- buildCallback cb
  parser' <- buildParser name parser
  pure $ Command names parent hidden checks params help parser' cb'

{- | Given the properties of a 'Command', a callback, and a type level list of
 the parameters, build a command by constructing a parser and wiring it up to
 the callback.

 ==== Examples

 Building a command that adds two numbers.

 @
 'buildCommand' \@\'['CalamityCommands.Parser.Named' "a" 'Int', 'CalamityCommands.Parser.Named' "b" 'Int']
    "add" 'Nothing' [] ('const' "Add two integers") $ \\ctx a b ->
      'pure' '$' 'Right' (a '+' b)
 @
-}
buildCommand ::
  forall ps c m a r.
  (Monad m, P.Member (P.Final m) r, TypedCommandC ps a r, CommandContext m c a) =>
  -- | The name (and aliases) of the command
  NonEmpty S.Text ->
  -- | The parent group of the command
  Maybe (Group m c a) ->
  -- | If the command is hidden
  Bool ->
  -- | The checks for the command
  [Check m c] ->
  -- | The help generator for this command
  (c -> L.Text) ->
  -- | The callback foor this command
  (c -> CommandForParsers ps r a) ->
  P.Sem r (Command m c a)
buildCommand names parent hidden checks help command =
  let (parser, cb) = buildTypedCommand @ps command
   in buildCommand' names parent hidden checks (parameterInfos @ps @r) help parser cb

{- | Given the name of the command the parser is for and a parser function in
 the 'P.Sem' monad, build a parser by transforming the Polysemy action into an
 @m@ action.
-}
buildParser ::
  (Monad m, P.Member (P.Final m) r) =>
  S.Text ->
  (c -> P.Sem r (Either CommandError a)) ->
  P.Sem r (c -> m (Either CommandError a))
buildParser name cb = do
  cb' <- bindSemToM cb
  let cb'' ctx = fromMaybe (Left $ ParseError ("Parser for command: " <> name) "failed internally") <$> cb' ctx
  pure cb''

{- | Given a callback for a command in the 'P.Sem' monad, build a command callback by
 transforming the Polysemy action into an @m@ action.
-}
buildCallback ::
  (Monad m, P.Member (P.Final m) r) => ((c, p) -> P.Sem (P.Fail ': r) a) -> P.Sem r ((c, p) -> m (Either L.Text a))
buildCallback cb = do
  cb' <- bindSemToM (\x -> P.runFail (cb x) <&> mapLeft L.pack)
  let cb'' = fromMaybe (Left "failed internally") <.> cb'
  pure cb''

-- | Given an invokation Context @c@, run a command. This does not perform the command's checks.
runCommand :: (Monad m, P.Member (P.Embed m) r) => c -> Command m c a -> P.Sem r (Either CommandError a)
runCommand ctx Command{names = name :| _, parser, callback} =
  P.embed (parser ctx) >>= \case
    Left e -> pure $ Left e
    Right p' -> P.embed (callback (ctx, p')) <&> mapLeft (InvokeError name)

{- | Given an invokation Context @c@, first run all of the command's checks, then
 run the command if they all pass.
-}
invokeCommand :: (Monad m, P.Member (P.Embed m) r) => c -> Command m c a -> P.Sem r (Either CommandError a)
invokeCommand ctx cmd@Command{checks} = P.runError $ do
  for_ checks (P.fromEither <=< runCheck ctx)
  P.fromEither =<< runCommand ctx cmd

type CommandSemType r a = P.Sem (P.Fail ': r) a

-- | Some constraints used for making parameter typed commands work
type TypedCommandC ps a r =
  ( ApplyTupRes (ParserResult (ListToTup ps)) (CommandSemType r a) ~ CommandForParsers ps r a
  , ParameterParser (ListToTup ps) r
  , ApplyTup (ParserResult (ListToTup ps)) (CommandSemType r a)
  , ParameterInfoForParsers ps r
  )

buildTypedCommand ::
  forall (ps :: [Type]) c m a p r.
  (TypedCommandC ps a r, p ~ ParserResult (ListToTup ps), CommandContext m c a) =>
  (c -> CommandForParsers ps r a) ->
  ( c -> P.Sem r (Either CommandError p)
  , (c, p) -> P.Sem (P.Fail ': r) a
  )
buildTypedCommand cmd =
  let parser ctx = buildTypedCommandParser @ps ctx (ctxUnparsedParams @m ctx)
      consumer (ctx, r) = applyTup (cmd ctx) r
   in (parser, consumer)

class ParameterInfoForParsers (ps :: [Type]) r where
  parameterInfos :: [ParameterInfo]

instance ParameterInfoForParsers '[] r where
  parameterInfos = []

instance (ParameterParser x r, ParameterInfoForParsers xs r) => ParameterInfoForParsers (x : xs) r where
  parameterInfos = parameterInfo @x @r : parameterInfos @xs @r

class ApplyTup a b where
  type ApplyTupRes a b

  applyTup :: ApplyTupRes a b -> a -> b

instance ApplyTup as b => ApplyTup (a, as) b where
  type ApplyTupRes (a, as) b = a -> ApplyTupRes as b

  applyTup f (a, as) = applyTup (f a) as

instance ApplyTup () b where
  type ApplyTupRes () b = b

  applyTup r () = r

buildTypedCommandParser ::
  forall (ps :: [Type]) c r.
  ParameterParser (ListToTup ps) r =>
  c ->
  L.Text ->
  P.Sem r (Either CommandError (ParserResult (ListToTup ps)))
buildTypedCommandParser ctx t =
  runCommandParser ctx t (parse @(ListToTup ps) @r) <&> \case
    Right r -> Right r
    Left (n, e) -> Left $ ParseError n e

type family ListToTup (ps :: [Type]) where
  ListToTup '[] = ()
  ListToTup (x ': xs) = (x, ListToTup xs)

{- | Transform a type level list of types implementing the 'Parser' typeclass into
 the type a command callback matching those parameters should be.

 As an example:

 @
 'CommandForParsers' [ 'L.Text', 'Int', 'CalamityCommands.Parser.Named' "something" 'L.Text' ] r a ~
   ('L.Text' -> 'Int' -> 'L.Text' -> 'P.Sem' r ('P.Fail' ': r) a)
 @
-}
type family CommandForParsers (ps :: [Type]) r a where
  CommandForParsers '[] r a = P.Sem (P.Fail ': r) a
  CommandForParsers (x ': xs) r a = ParserResult x -> CommandForParsers xs r a
