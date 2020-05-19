-- | Commands and stuff
module Calamity.Commands.Command
    ( Command(..)
    , buildCommand
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

import           Control.Lens                   hiding ( (<.>), Context )
import           Control.Monad

import           Data.Foldable
import           Data.Kind
import           Data.Maybe
import           Data.Text                      as S
import           Data.Text.Lazy                 as L
import           Data.Typeable

import qualified Polysemy                       as P
import qualified Polysemy.Error                 as P
import qualified Polysemy.Fail                  as P

data Command = forall a. MkCommand
  { name     :: S.Text
  , parent   :: Maybe Group
  , checks   :: [Check]
  , help     :: Context -> L.Text
  , parser   :: Context -> Either CommandError a
  , callback :: (Context, a) -> IO (Maybe L.Text)
  }

buildCommand :: P.Member (P.Final IO) r
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> L.Text)
             -> (Context -> Either CommandError a)
             -> ((Context, a) -> P.Sem (P.Fail ': r) ())
             -> P.Sem r Command
buildCommand name parent checks help parser cb = do
  cb' <- buildCallback cb
  pure $ MkCommand name parent checks help parser cb'

buildCallback
  :: P.Member (P.Final IO) r => ((Context, a) -> P.Sem (P.Fail ': r) ()) -> P.Sem r ((Context, a) -> IO (Maybe L.Text))
buildCallback cb = do
  cb' <- bindSemToIO (\x -> P.runFail (cb x) <&> \case
                        Left e -> Just $ L.pack e
                        _      -> Nothing)
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure cb''

runCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
runCommand ctx MkCommand { name, parser, callback } =
  case parser ctx of
    Left e   -> pure $ Left e
    Right p' -> P.embed (callback (ctx, p')) <&> justToEither . (InvokeError name <$>)

invokeCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Maybe CommandError)
invokeCommand ctx cmd@MkCommand { checks } =
  let r = P.runError $ do
        for_ checks (P.fromEither <=< runCheck ctx)
        P.fromEither =<< runCommand ctx cmd
  in leftToMaybe <$> r

-- buildTypedCommand :: forall (ps :: [Type]) r a. CommandForParsers ps a -> (Context -> Either CommandError a, (Context, a) -> P.Sem (P.Fail ': r) ())
-- buildTypedCommand cmd = undefined

class BuildTypedCommandParser (ps :: [Type]) where
  buildTypedCommandParser :: (Context, L.Text) -> Either CommandError (ParamsFromParsers ps)

instance BuildTypedCommandParser '[] where
  buildTypedCommandParser (ctx, _) = Right ()

instance (Typeable x, Parser x, BuildTypedCommandParser xs) => BuildTypedCommandParser (x ': xs) where
  buildTypedCommandParser (ctx, msg) = case parse @x (ctx, msg) of
    Right (a, msg') -> case buildTypedCommandParser @xs (ctx, msg') of
      Right as -> Right (a, as)
      Left e   -> Left e
    Left e -> Left (ParseError (S.pack . show . typeRep $ Proxy @x) msg)

type family ParamsFromParsers (ps :: [Type]) where
  ParamsFromParsers '[] = ()
  ParamsFromParsers (x ': xs) = (ParserResult x, ParamsFromParsers xs)

type family CommandForParsers (ps :: [Type]) a where
  CommandForParsers '[] a       = () -> Either CommandError a
  CommandForParsers (x ': xs) a = ParserResult x -> CommandForParsers xs a
