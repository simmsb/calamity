-- | Commands and stuff
module Calamity.Commands.Command
    ( Command(..)
    , TypedCommandC
    , CommandForParsers
    , buildCommand
    , buildCommand'
    , buildCallback
    , runCommand
    , invokeCommand ) where

import           Calamity.Commands.Check
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

import           GHC.Generics

import qualified Polysemy                    as P
import qualified Polysemy.Error              as P
import qualified Polysemy.Fail               as P

import           TextShow
import qualified TextShow.Generic            as TSG

data Command = forall a. Command
  { name     :: S.Text
  , parent   :: Maybe Group
  , checks   :: [Check]
  , help     :: Context -> L.Text
  , parser   :: Context -> Either CommandError a
  , callback :: (Context, a) -> IO (Maybe L.Text)
  }

data CommandS = CommandS
  { name :: S.Text
  , parent :: Maybe S.Text
  , checks :: [S.Text]
  }
  deriving ( Generic, Show )
  deriving ( TextShow ) via TSG.FromGeneric CommandS

instance Show Command where
  showsPrec d Command { name, parent, checks } = showsPrec d $ CommandS name (parent ^? _Just . #name) (checks ^.. traverse . #name)

instance TextShow Command where
  showbPrec d Command { name, parent, checks } = showbPrec d $ CommandS name (parent ^? _Just . #name) (checks ^.. traverse . #name)

buildCommand' :: P.Member (P.Final IO) r
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> L.Text)
             -> (Context -> Either CommandError a)
             -> ((Context, a) -> P.Sem (P.Fail ': r) ())
             -> P.Sem r Command
buildCommand' name parent checks help parser cb = do
  cb' <- buildCallback cb
  pure $ Command name parent checks help parser cb'

buildCommand :: forall ps a r. (P.Member (P.Final IO) r,
                                TypedCommandC ps a (P.Sem (P.Fail ': r) ()))
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> L.Text)
             -> (Context -> CommandForParsers ps (P.Sem (P.Fail ': r) ()))
             -> P.Sem r Command
buildCommand name parent checks help command =
  let (parser, cb) = buildTypedCommand @ps command
  in buildCommand' name parent checks help parser cb

buildCallback
  :: P.Member (P.Final IO) r => ((Context, a) -> P.Sem (P.Fail ': r) ()) -> P.Sem r ((Context, a) -> IO (Maybe L.Text))
buildCallback cb = do
  cb' <- bindSemToIO (\x -> P.runFail (cb x) <&> \case
                        Left e -> Just $ L.pack e
                        _      -> Nothing)
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure cb''

runCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
runCommand ctx Command { name, parser, callback } =
  case parser ctx of
    Left e   -> pure $ Left e
    Right p' -> P.embed (callback (ctx, p')) <&> justToEither . (InvokeError name <$>)

invokeCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Maybe CommandError)
invokeCommand ctx cmd@Command { checks } =
  let r = P.runError $ do
        for_ checks (P.fromEither <=< runCheck ctx)
        P.fromEither =<< runCommand ctx cmd
  in leftToMaybe <$> r

type TypedCommandC ps a b =
  (ApplyTupRes a b ~ CommandForParsers ps b, a ~ ParamsFromParsers ps, BuildTypedCommandParser ps, ApplyTup a b)

buildTypedCommand :: forall (ps :: [Type]) a b.
  TypedCommandC ps a b
  => (Context -> CommandForParsers ps b)
  -> (Context -> Either CommandError a, (Context, a) -> b)
buildTypedCommand cmd =
  let parser ctx = buildTypedCommandParser @ps (ctx, ctx ^. #unparsedMessage)
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

class BuildTypedCommandParser (ps :: [Type]) where
  buildTypedCommandParser :: (Context, L.Text) -> Either CommandError (ParamsFromParsers ps)

instance BuildTypedCommandParser '[] where
  buildTypedCommandParser (_, _) = Right ()

instance (Typeable x, Parser x, BuildTypedCommandParser xs) => BuildTypedCommandParser (x ': xs) where
  buildTypedCommandParser (ctx, msg) = case parse @x (ctx, msg) of
    Right (a, msg') -> (a,) <$> buildTypedCommandParser @xs (ctx, msg')
    Left e -> Left (ParseError (S.pack . show . typeRep $ Proxy @x) e)

type family ParamsFromParsers (ps :: [Type]) where
  ParamsFromParsers '[] = ()
  ParamsFromParsers (x ': xs) = (ParserResult x, ParamsFromParsers xs)

type family CommandForParsers (ps :: [Type]) a where
  CommandForParsers '[] a       = a
  CommandForParsers (x ': xs) a = ParserResult x -> CommandForParsers xs a
