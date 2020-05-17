{-# LANGUAGE TemplateHaskell #-}

-- | Something for converting polysemy actions into IO actions
module Calamity.Internal.RunIntoIO
    ( IntoIO(..)
    , runIntoIOFinal
    , intoIO
    , bindSemToIO ) where

import           Data.Functor

import qualified Polysemy                         as P
import qualified Polysemy.Final                   as P

data IntoIO p r m a where
  IntoIO :: (p -> m r) -> IntoIO p r m (p -> IO (Maybe r))

runIntoIOFinal :: forall r p b a. P.Member (P.Final IO) r => P.Sem (IntoIO p b ': r) a -> P.Sem r a
runIntoIOFinal = P.interpretFinal $ \case
  IntoIO m -> do
    istate <- P.getInitialStateS
    m' <- P.bindS m
    ins <- P.getInspectorS
    P.liftS $ pure (\x -> P.inspect ins <$> m' (istate $> x))

P.makeSem ''IntoIO

bindSemToIO :: forall r p a. P.Member (P.Final IO) r => (p -> P.Sem r a) -> P.Sem r (p -> IO (Maybe a))
bindSemToIO f = runIntoIOFinal go
  where go :: P.Sem (IntoIO p a ': r) (p -> IO (Maybe a))
        go = intoIO (P.raise . f)
