-- | Command invokation preconditions
module Calamity.Commands.Check
    ( Check(..)
    , buildCheck
    , buildCheckPure
    , runCheck ) where

import           Calamity.Commands.Context
import           Calamity.Commands.Error
import           Calamity.Internal.RunIntoIO
import           Calamity.Internal.Utils

import           Control.Lens                hiding ( (<.>), Context )

import           Data.Maybe
import qualified Data.Text                   as S
import qualified Data.Text.Lazy              as L

import           GHC.Generics

import qualified Polysemy                    as P

data Check = MkCheck
  { name     :: S.Text
  , callback :: Context -> IO (Maybe L.Text)
  }
  deriving ( Generic )

buildCheck :: P.Member (P.Final IO) r => S.Text -> (Context -> P.Sem r (Maybe L.Text)) -> P.Sem r Check
buildCheck name cb = do
  cb' <- bindSemToIO cb
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure $ MkCheck name cb''

buildCheckPure :: S.Text -> (Context -> Maybe L.Text) -> Check
buildCheckPure name cb = MkCheck name (pure . cb)

runCheck :: P.Member (P.Embed IO) r => Context -> Check -> P.Sem r (Either CommandError ())
runCheck ctx chk = P.embed (callback chk ctx) <&> justToEither . (CheckError (chk ^. #name) <$>)
