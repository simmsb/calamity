{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal utilities and instances
module Calamity.Internal.Utils
    ( whileMFinalIO
    , untilJustFinalIO
    , whenJust
    , whenM
    , unlessM
    , lastMaybe
    , leftToMaybe
    , rightToMaybe
    , justToEither
    , (<<$>>)
    , (<<*>>)
    , (<.>)
    , debug
    , info
    , Calamity.Internal.Utils.error
    , swap ) where

import           Calamity.Types.LogEff
import           Calamity.Internal.RunIntoIO

import           Control.Applicative

import           Data.Default.Class
import qualified Data.HashMap.Lazy     as LH
import qualified Data.Map              as M
import           Data.Semigroup        ( Last(..) )
import           Data.Text.Lazy
import           Data.Time
import qualified Data.Vector.Unboxing  as VU
import           Data.Vector.Unboxing  ( Vector )
import           Data.Aeson

import qualified DiPolysemy            as Di

import qualified Polysemy              as P

import           TextShow
import Data.Colour (Colour)

-- | Like whileM, but stateful effects are not preserved to mitigate memory leaks
--
-- This means Polysemy.Error won't work to break the loop, etc.
-- Instead, Error/Alternative will just result in the loop quitting.
whileMFinalIO :: P.Member (P.Final IO) r => P.Sem r Bool -> P.Sem r ()
whileMFinalIO action = do
  action' <- runSemToIO action
  P.embedFinal $ go action'
  where go action' = do
          r <- action'
          case r of
            Just True ->
              go action'
            _ ->
              pure ()

-- | Like untilJust, but stateful effects are not preserved to mitigate memory leaks
--
-- This means Polysemy.Error won't work to break the loop, etc.
-- Instead, Error/Alternative will just result in another loop.
untilJustFinalIO :: P.Member (P.Final IO) r => P.Sem r (Maybe a) -> P.Sem r a
untilJustFinalIO action = do
  action' <- runSemToIO action
  P.embedFinal $ go action'
  where go action' = do
          r <- action'
          case r of
            Just (Just a) ->
              pure a
            _ ->
              go action'

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \case
  True  -> m
  False -> pure ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = whenM . (not <$>)

lastMaybe :: Maybe a -> Maybe a -> Maybe a
lastMaybe l r = getLast <$> fmap Last l <> fmap Last r

leftToMaybe :: Either e a -> Maybe e
leftToMaybe (Left x) = Just x
leftToMaybe _        = Nothing

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe _         = Nothing

justToEither :: Maybe e -> Either e ()
justToEither (Just x) = Left x
justToEither _        = Right ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

infixl 4 <<*>>

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
(<.>) f g x = f <$> g x

infixl 4 <.>

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

instance (TextShow a, VU.Unboxable a) => TextShow (Vector a) where
  showb = showbList . VU.toList

instance (Show k, Show v) => TextShow (LH.HashMap k v) where
  showb = fromString . show

instance (Show k, Show v) => TextShow (M.Map k v) where
  showb = fromString . show

instance (Show a, Fractional a) => TextShow (Colour a) where
  showb = fromString . show

instance Default (M.Map k v) where
    def = M.empty

instance (FromJSON a, VU.Unboxable a) => FromJSON (VU.Vector a) where
  parseJSON = (VU.fromList <$>) . parseJSON

instance (ToJSON a, VU.Unboxable a) => ToJSON (VU.Vector a) where
  toJSON = toJSON . VU.toList
  toEncoding = toEncoding . VU.toList
