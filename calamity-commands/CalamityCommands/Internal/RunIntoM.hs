{-# LANGUAGE TemplateHaskell #-}

-- | Something for converting polysemy actions into monadic actions
module CalamityCommands.Internal.RunIntoM
    ( runSemToM
    , bindSemToM ) where

import           Data.Functor

import qualified Polysemy                         as P
import qualified Polysemy.Final                   as P

runSemToM :: forall m r a. (Monad m, P.Member (P.Final m) r) => P.Sem r a -> P.Sem r (m (Maybe a))
runSemToM m = P.withStrategicToFinal $ do
  m' <- P.runS m
  ins <- P.getInspectorS
  P.liftS $ pure (P.inspect ins <$> m')

bindSemToM :: forall m r p a. (Monad m, P.Member (P.Final m) r) => (p -> P.Sem r a) -> P.Sem r (p -> m (Maybe a))
bindSemToM m = P.withStrategicToFinal $ do
  istate <- P.getInitialStateS
  m' <- P.bindS m
  ins <- P.getInspectorS
  P.liftS $ pure (\x -> P.inspect ins <$> m' (istate $> x))
