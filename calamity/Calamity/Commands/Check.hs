-- | Command invokation preconditions
module Calamity.Commands.Check
    ( Check(..)
    , buildCheck
    , buildCheckPure
    , runCheck ) where

import {-# SOURCE #-} Calamity.Commands.Context
import           Calamity.Commands.Error
import           Calamity.Internal.RunIntoIO
import           Calamity.Internal.Utils

import           Control.Lens                hiding ( (<.>), Context )

import           Data.Generics.Labels        ()
import           Data.Maybe
import qualified Data.Text                   as S
import qualified Data.Text.Lazy              as L

import           GHC.Generics

import qualified Polysemy                    as P

-- | A check for a command.
--
-- Every check for a command must return Nothing for the command to be run.
data Check = MkCheck
  { name     :: S.Text
    -- ^ The name of the check.
  , callback :: Context -> IO (Maybe L.Text)
    -- ^ The callback for the check, returns Nothing if it passes, otherwise
    -- returns the reason for it not passing.
  }
  deriving ( Generic )

-- | Given the name of a check and a callback in the 'P.Sem' monad, build a
-- check by transforming the Polysemy action into an IO action.
buildCheck :: P.Member (P.Final IO) r => S.Text -> (Context -> P.Sem r (Maybe L.Text)) -> P.Sem r Check
buildCheck name cb = do
  cb' <- bindSemToIO cb
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure $ MkCheck name cb''

-- | Given the name of a check and a pure callback function, build a check.
buildCheckPure :: S.Text -> (Context -> Maybe L.Text) -> Check
buildCheckPure name cb = MkCheck name (pure . cb)

-- | Given an invokation 'Context', run a check and transform the result into an
-- @'Either' 'CommandError' ()@.
runCheck :: P.Member (P.Embed IO) r => Context -> Check -> P.Sem r (Either CommandError ())
runCheck ctx chk = P.embed (callback chk ctx) <&> justToEither . (CheckError (chk ^. #name) <$>)
