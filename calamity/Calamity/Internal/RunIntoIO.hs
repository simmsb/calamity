-- | Something for converting polysemy actions into IO actions
module Calamity.Internal.RunIntoIO (
  runSemToIO,
  bindSemToIO,
) where

import Data.Functor

import Polysemy qualified as P
import Polysemy.Final qualified as P

runSemToIO :: forall r a. P.Member (P.Final IO) r => P.Sem r a -> P.Sem r (IO (Maybe a))
runSemToIO m = P.withStrategicToFinal $ do
  m' <- P.runS m
  ins <- P.getInspectorS
  P.liftS $ pure (P.inspect ins <$> m')

bindSemToIO :: forall r p a. P.Member (P.Final IO) r => (p -> P.Sem r a) -> P.Sem r (p -> IO (Maybe a))
bindSemToIO m = P.withStrategicToFinal $ do
  istate <- P.getInitialStateS
  m' <- P.bindS m
  ins <- P.getInspectorS
  P.liftS $ pure (\x -> P.inspect ins <$> m' (istate $> x))
