{-# LANGUAGE TemplateHaskell #-}

-- | A Writer monad that supports local writing, reverse reader I guess?
module Calamity.Internal.LocalWriter (
  LocalWriter (..),
  ltell,
  llisten,
  runLocalWriter,
) where

import Polysemy qualified as P
import Polysemy.State qualified as P

data LocalWriter o m a where
  Ltell :: o -> LocalWriter o m ()
  Llisten :: m a -> LocalWriter o m (o, a)

P.makeSem ''LocalWriter

runLocalWriter :: Monoid o => P.Sem (LocalWriter o ': r) a -> P.Sem r (o, a)
runLocalWriter =
  P.runState mempty
    . P.reinterpretH
      ( \case
          Ltell o -> do
            P.modify' (<> o) >>= P.pureT
          Llisten m -> do
            mm <- P.runT m
            (o, fa) <- P.raise $ runLocalWriter mm
            pure $ fmap (o,) fa
      )
