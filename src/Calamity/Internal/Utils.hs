{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal utilities and instances
module Calamity.Internal.Utils
    ( whenJust
    , whenM
    , unlessM
    , lastMaybe
    , (<<$>>)
    , debug
    , info
    , Calamity.Internal.Utils.error
    , swap ) where

import           Calamity.LogEff

import           Data.Default.Class
import qualified Data.HashMap.Lazy   as LH
import qualified Data.Map            as M
import           Data.Semigroup      ( Last(..) )
import           Data.Text.Lazy
import           Data.Time
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed ( Vector )

import qualified DiPolysemy          as Di

import qualified Polysemy            as P

import           TextShow

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \case
  True -> m
  _    -> pure ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = whenM . (not <$>)

lastMaybe :: Maybe a -> Maybe a -> Maybe a
lastMaybe l r = getLast <$> fmap Last l <> fmap Last r

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

debug :: P.Member LogEff r => Text -> P.Sem r ()
debug = Di.debug

info :: P.Member LogEff r => Text -> P.Sem r ()
info = Di.info

error :: P.Member LogEff r => Text -> P.Sem r ()
error = Di.error

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)

instance TextShow UTCTime where
  showb = fromString . show

instance (TextShow a, VU.Unbox a) => TextShow (Vector a) where
  showb = showbList . VU.toList

    -- lazy and strict use the same internal data structure
instance (Show k, Show v) => TextShow (LH.HashMap k v) where
  showb = fromString . show

instance (Show k, Show v) => TextShow (M.Map k v) where
  showb = fromString . show

instance Default (M.Map k v) where
  def = M.empty
