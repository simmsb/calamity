{-# LANGUAGE TemplateHaskell #-}

-- | Command invokation preconditions
module CalamityCommands.Check (
  Check (..),
  buildCheck,
  buildCheckPure,
  runCheck,
) where

import CalamityCommands.Error
import CalamityCommands.Internal.RunIntoM
import CalamityCommands.Internal.Utils
import Data.Maybe
import qualified Data.Text as T
import Optics
import qualified Polysemy as P

{- | A check for a command.

 Every check for a command must return Nothing for the command to be run.
-}
data Check m c = MkCheck
  { -- | The name of the check.
    name :: T.Text
  , -- | The callback for the check, returns Nothing if it passes, otherwise
    -- returns the reason for it not passing.
    callback :: c -> m (Maybe T.Text)
  }

$(makeFieldLabelsNoPrefix ''Check)

{- | Given the name of a check and a callback in the 'P.Sem' monad, build a check
 by transforming the Polysemy action into an @m@ action.
-}
buildCheck :: (Monad m, P.Member (P.Final m) r) => T.Text -> (c -> P.Sem r (Maybe T.Text)) -> P.Sem r (Check m c)
buildCheck name cb = do
  cb' <- bindSemToM cb
  let cb'' = fromMaybe (Just "failed internally") <.> cb'
  pure $ MkCheck name cb''

-- | Given the name of a check and a pure callback function, build a check.
buildCheckPure :: Monad m => T.Text -> (c -> Maybe T.Text) -> Check m c
buildCheckPure name cb = MkCheck name (pure . cb)

{- | Given an invokation context @c@, run a check and transform the result into an
 @'Either' 'CommandError' ()@.
-}
runCheck :: (Monad m, P.Member (P.Embed m) r) => c -> Check m c -> P.Sem r (Either CommandError ())
runCheck ctx chk = P.embed (callback chk ctx) <&> justToEither . (CheckError (chk ^. #name) <$>)
