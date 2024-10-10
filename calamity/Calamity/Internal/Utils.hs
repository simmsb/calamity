{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- | Internal utilities and instances
module Calamity.Internal.Utils (
  whileMFinalIO,
  untilJustFinalIO,
  whenJust,
  whenM,
  unlessM,
  lastMaybe,
  leftToMaybe,
  rightToMaybe,
  justToEither,
  (<<$>>),
  (<<*>>),
  (<.>),
  (.?=),
  (.=),
  debug,
  info,
  Calamity.Internal.Utils.error,
  swap,
  DefaultingMap (..),
  AesonVector (..),
  CalamityFromStringShow (..),
  MaybeNull (..),
  CalamityToJSON (..),
  CalamityToJSON' (..),
) where

import Calamity.Internal.RunIntoIO
import Calamity.Types.LogEff
import Control.Applicative
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (null_)
import Data.Default.Class
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Semigroup (Last (..))
import Data.Text
import Data.Vector.Unboxing qualified as VU
import DiPolysemy qualified as Di
import Polysemy qualified as P
import TextShow

{- | Like whileM, but stateful effects are not preserved to mitigate memory leaks

 This means Polysemy.Error won't work to break the loop, etc.
 Instead, Error/Alternative will just result in the loop quitting.
-}
whileMFinalIO :: (P.Member (P.Final IO) r) => P.Sem r Bool -> P.Sem r ()
whileMFinalIO action = do
  action' <- runSemToIO action
  P.embedFinal $ go action'
  where
    go action' = do
      r <- action'
      case r of
        Just True ->
          go action'
        _ ->
          pure ()

{- | Like untilJust, but stateful effects are not preserved to mitigate memory leaks

 This means Polysemy.Error won't work to break the loop, etc.
 Instead, Error/Alternative will just result in another loop.
-}
untilJustFinalIO :: (P.Member (P.Final IO) r) => P.Sem r (Maybe a) -> P.Sem r a
untilJustFinalIO action = do
  action' <- runSemToIO action
  P.embedFinal $ go action'
  where
    go action' = do
      r <- action'
      case r of
        Just (Just a) ->
          pure a
        _ ->
          go action'

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p m =
  p >>= \case
    True -> m
    False -> pure ()

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM = whenM . (not <$>)

lastMaybe :: Maybe a -> Maybe a -> Maybe a
lastMaybe l r = getLast <$> fmap Last l <> fmap Last r

leftToMaybe :: Either e a -> Maybe e
leftToMaybe (Left x) = Just x
leftToMaybe _ = Nothing

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe _ = Nothing

justToEither :: Maybe e -> Either e ()
justToEither (Just x) = Left x
justToEither _ = Right ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

infixl 4 <<*>>

(<.>) :: (Functor f) => (a -> b) -> (c -> f a) -> (c -> f b)
(<.>) f g x = f <$> g x

infixl 4 <.>

debug :: (P.Member LogEff r) => Text -> P.Sem r ()
debug = Di.debug

info :: (P.Member LogEff r) => Text -> P.Sem r ()
info = Di.info

error :: (P.Member LogEff r) => Text -> P.Sem r ()
error = Di.error

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)

newtype DefaultingMap k v = DefaultingMap {unDefaultingMap :: M.Map k v}

instance Default (DefaultingMap k v) where
  def = DefaultingMap M.empty

newtype AesonVector a = AesonVector {unAesonVector :: VU.Vector a}
  deriving (Show) via VU.Vector a

instance (Aeson.FromJSON a, VU.Unboxable a) => Aeson.FromJSON (AesonVector a) where
  parseJSON = (AesonVector . VU.fromList <$>) . Aeson.parseJSON

instance (Aeson.ToJSON a, VU.Unboxable a) => Aeson.ToJSON (AesonVector a) where
  toJSON = Aeson.toJSON . VU.toList . unAesonVector
  toEncoding = Aeson.toEncoding . VU.toList . unAesonVector

instance (TextShow a, VU.Unboxable a) => TextShow (AesonVector a) where
  showb = showbList . VU.toList . unAesonVector

newtype CalamityFromStringShow a = CalamityFromStringShow {unCalamityFromStringShow :: a}
  deriving (Aeson.FromJSON, Aeson.ToJSON) via a
  deriving (TextShow) via FromStringShow a

{- | An alternative 'Maybe' type that allows us to distinguish between parsed
 json fields that were null, and fields that didn't exist.
-}
data MaybeNull a
  = WasNull
  | NotNull a
  deriving (Show)

instance (Aeson.FromJSON a) => Aeson.FromJSON (MaybeNull a) where
  parseJSON Aeson.Null = pure WasNull
  parseJSON x = NotNull <$> Aeson.parseJSON x

instance (Aeson.ToJSON a) => Aeson.ToJSON (MaybeNull a) where
  toJSON WasNull = Aeson.Null
  toJSON (NotNull x) = Aeson.toJSON x

  toEncoding WasNull = null_
  toEncoding (NotNull x) = Aeson.toEncoding x

#if MIN_VERSION_aeson(2,2,0)
(.?=) :: (Aeson.ToJSON v, Aeson.KeyValue e kv) => Aeson.Key -> Maybe v -> Maybe kv
k .?= Just v = Just (k Aeson..= v)
_ .?= Nothing = Nothing

(.=) :: (Aeson.ToJSON v, Aeson.KeyValue e kv) => Aeson.Key -> v -> Maybe kv
k .= v = Just (k Aeson..= v)

class CalamityToJSON' a where
  toPairs :: Aeson.KeyValue v kv => a -> [Maybe kv]
#else
(.?=) :: (Aeson.ToJSON v, Aeson.KeyValue kv) => Aeson.Key -> Maybe v -> Maybe kv
k .?= Just v = Just (k Aeson..= v)
_ .?= Nothing = Nothing

(.=) :: (Aeson.ToJSON v, Aeson.KeyValue kv) => Aeson.Key -> v -> Maybe kv
k .= v = Just (k Aeson..= v)

class CalamityToJSON' a where
  toPairs :: Aeson.KeyValue kv => a -> [Maybe kv]
#endif

newtype CalamityToJSON a = CalamityToJSON a

instance (CalamityToJSON' a) => Aeson.ToJSON (CalamityToJSON a) where
  toJSON (CalamityToJSON x) = Aeson.object . catMaybes . toPairs $ x
  toEncoding (CalamityToJSON x) = Aeson.pairs . mconcat . catMaybes . toPairs $ x
