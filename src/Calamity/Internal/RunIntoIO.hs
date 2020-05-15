{-# LANGUAGE TemplateHaskell #-}

-- | Something for converting polysemy actions into IO actions
module Calamity.Internal.RunIntoIO
    ( IntoIO(..)
    , runIntoIOFinal
    , intoIO ) where

import           Control.Monad

import           Data.Functor

import qualified Polysemy                         as P
import qualified Polysemy.Final                   as P

data IntoIO p m a where
  IntoIO :: (p -> m ()) -> IntoIO p m (p -> IO ())

runIntoIOFinal :: forall r p a. P.Member (P.Final IO) r => P.Sem (IntoIO p ': r) a -> P.Sem r a
runIntoIOFinal = P.interpretFinal $ \case
  IntoIO m -> do
    istate <- P.getInitialStateS
    m' <- P.bindS m
    P.liftS $ pure (\x -> void (m' $ istate $> x))

P.makeSem ''IntoIO
