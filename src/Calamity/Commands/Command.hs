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
import           Calamity.Internal.RunIntoIO
import           Calamity.Internal.Utils

import           Control.Lens                hiding ( (<.>), Context )
import           Control.Monad

import           Data.Foldable
import           Data.Maybe
import           Data.Text                   as S
import           Data.Text.Lazy              as L

import qualified Polysemy                    as P
import qualified Polysemy.Error              as P
import qualified Polysemy.Fail               as P

data Command = forall a. MkCommand
  { name     :: S.Text
  , parent   :: Maybe Group
  , checks   :: [Check]
  , help     :: Context -> L.Text
  , parser   :: Context -> IO (Either CommandError a)
  , callback :: (Context, a) -> IO (Maybe L.Text)
  }

buildCommand :: P.Member (P.Final IO) r
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> L.Text)
             -> (Context -> P.Sem r (Either CommandError a))
             -> ((Context, a) -> P.Sem (P.Fail ': r) ())
             -> P.Sem r Command
buildCommand name parent checks help parser cb = do
  cb' <- buildCallback cb
  parser' <- buildParser name parser
  pure $ MkCommand name parent checks help parser' cb'

buildParser :: P.Member (P.Final IO) r
            => S.Text
            -> (Context -> P.Sem r (Either CommandError a))
            -> P.Sem r (Context -> IO (Either CommandError a))
buildParser cmdName cb = do
  cb' <- bindSemToIO cb
  let cb'' ctx = fromMaybe
        (Left $ ParseError ("Parser for command: " <> cmdName) "failed internally" ("", ctx ^. #message . #content))
        <$> cb' ctx
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
runCommand ctx MkCommand { name, parser, callback } = do
  p <- P.embed $ parser ctx
  case p of
    Left e   -> pure $ Left e
    Right p' -> P.embed (callback (ctx, p')) <&> justToEither . (InvokeError name <$>)

invokeCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Maybe CommandError)
invokeCommand ctx cmd@MkCommand { checks } =
  let r = P.runError $ do
        for_ checks (P.fromEither <=< runCheck ctx)
        P.fromEither =<< runCommand ctx cmd
  in leftToMaybe <$> r
