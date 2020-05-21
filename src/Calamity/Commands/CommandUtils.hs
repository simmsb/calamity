-- | Command utilities
module Calamity.Commands.CommandUtils
    ( TypedCommandC
    , CommandForParsers
    , buildCommand
    , buildCommand'
    , buildParser
    , buildCallback
    , runCommand
    , invokeCommand ) where

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
import           Data.Typeable

import qualified Polysemy                    as P
import qualified Polysemy.Error              as P
import qualified Polysemy.Fail               as P

buildCommand' :: P.Member (P.Final IO) r
              => S.Text
              -> Maybe Group
              -> [Check]
              -> (Context -> L.Text)
              -> (Context -> P.Sem r (Either CommandError a))
              -> ((Context, a) -> P.Sem (P.Fail ': r) ())
              -> P.Sem r Command
buildCommand' name parent checks help parser cb = do
  cb' <- buildCallback cb
  parser' <- buildParser name parser
  pure $ Command name parent checks help parser' cb'

buildCommand :: forall ps a r.
             (P.Member (P.Final IO) r, TypedCommandC ps a r)
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> L.Text)
             -> (Context -> CommandForParsers ps r)
             -> P.Sem r Command
buildCommand name parent checks help command = let (parser, cb) = buildTypedCommand @ps command
                                               in buildCommand' name parent checks help parser cb

buildParser :: P.Member (P.Final IO) r
            => S.Text
            -> (Context -> P.Sem r (Either CommandError a))
            -> P.Sem r (Context -> IO (Either CommandError a))
buildParser cmdName cb = do
  cb' <- bindSemToIO cb
  let cb'' ctx = fromMaybe (Left $ ParseError ("Parser for command: " <> cmdName) "failed internally") <$> cb' ctx
  pure cb''

buildCallback
  :: P.Member (P.Final IO) r => ((Context, a) -> P.Sem (P.Fail ': r) ()) -> P.Sem r ((Context, a) -> IO (Maybe L.Text))
buildCallback cb = do
  cb' <- bindSemToIO (\x -> P.runFail (cb x) <&> \case
                        Left e -> Just $ L.pack e
                        _      -> Nothing)
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure cb''

runCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
runCommand ctx Command { name, parser, callback } = P.embed (parser ctx) >>= \case
  Left e   -> pure $ Left e
  Right p' -> P.embed (callback (ctx, p')) <&> justToEither . (InvokeError name <$>)

invokeCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
invokeCommand ctx cmd@Command { checks } = P.runError $ do
  for_ checks (P.fromEither <=< runCheck ctx)
  P.fromEither =<< runCommand ctx cmd

type CommandSemType r = P.Sem (P.Fail ': r) ()

type TypedCommandC ps a r =
  ( ApplyTupRes a (CommandSemType r) ~ CommandForParsers ps r
  , a ~ ParamsFromParsers ps
  , BuildTypedCommandParser ps r
  , ApplyTup a (CommandSemType r))

buildTypedCommand
  :: forall (ps :: [Type]) a r.
  TypedCommandC ps a r
  => (Context -> CommandForParsers ps r)
  -> ( Context
         -> P.Sem r (Either CommandError a)
     , (Context, a)
         -> P.Sem (P.Fail ': r) ())
buildTypedCommand cmd = let parser ctx = buildTypedCommandParser @ps (ctx, ctx ^. #unparsedMessage)
                            consumer (ctx, r) = applyTup (cmd ctx) r
                        in (parser, consumer)

class ApplyTup a b where
  type ApplyTupRes a b

  applyTup :: ApplyTupRes a b -> a -> b

instance ApplyTup as b => ApplyTup (a, as) b where
  type ApplyTupRes (a, as) b = a -> ApplyTupRes as b

  applyTup f (a, as) = applyTup (f a) as

instance ApplyTup () b where
  type ApplyTupRes () b = b

  applyTup r () = r

class BuildTypedCommandParser (ps :: [Type]) r where
  buildTypedCommandParser :: (Context, L.Text) -> P.Sem r (Either CommandError (ParamsFromParsers ps))

instance BuildTypedCommandParser '[] r where
  buildTypedCommandParser (_, _) = pure $ Right ()

instance (Typeable x, Parser x r, BuildTypedCommandParser xs r) => BuildTypedCommandParser (x ': xs) r where
  buildTypedCommandParser (ctx, msg) = parse @x (ctx, msg) >>= \case
    Right (a, msg') -> (a, ) <<$>> buildTypedCommandParser @xs (ctx, msg')
    Left e          -> pure $ Left (ParseError (S.pack . show . typeRep $ Proxy @x) e)

type family ParamsFromParsers (ps :: [Type]) where
  ParamsFromParsers '[] = ()
  ParamsFromParsers (x ': xs) = (ParserResult x, ParamsFromParsers xs)

type family CommandForParsers (ps :: [Type]) r where
  CommandForParsers '[] r = P.Sem (P.Fail ': r) ()
  CommandForParsers (x ': xs) r = ParserResult x -> CommandForParsers xs r
