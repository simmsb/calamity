-- | Commands and stuff
module Calamity.Commands.Command
    ( Command(..)
    , buildCommand
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

import           GHC.Generics

import qualified Polysemy                    as P
import qualified Polysemy.Error              as P
import qualified Polysemy.Fail               as P

data Command = MkCommand
  { name   :: S.Text
  , parent :: Maybe Group
  , checks :: [Check]
  , invoke :: Context -> IO (Maybe L.Text)
  }
  deriving ( Generic )

buildCommand :: P.Member (P.Final IO) r
             => S.Text
             -> Maybe Group
             -> [Check]
             -> (Context -> P.Sem (P.Fail ': r) ())
             -> P.Sem r Command
buildCommand name parent checks cb = do
  cb' <- bindSemToIO (\x -> P.runFail (cb x) <&> \case
                        Left e -> Just $ L.pack e
                        _      -> Nothing)
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure $ MkCommand name parent checks cb''

runCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Either CommandError ())
runCommand ctx cmd = P.embed (invoke cmd ctx) <&> justToEither . (InvokeError (cmd ^. #name) <$>)

invokeCommand :: P.Member (P.Embed IO) r => Context -> Command -> P.Sem r (Maybe CommandError)
invokeCommand ctx cmd = let r = P.runError $ do
                              for_ (cmd ^. #checks) (P.fromEither <=< runCheck ctx)
                              P.fromEither =<< runCommand ctx cmd
                        in leftToMaybe <$> r
