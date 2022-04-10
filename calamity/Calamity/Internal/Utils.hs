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
  debug,
  info,
  Calamity.Internal.Utils.error,
  swap,
  DefaultingMap (..),
  AesonVector (..),
  CalamityFromStringShow (..),
  MaybeNull (..),
) where

import Calamity.Internal.RunIntoIO
import Calamity.Types.LogEff
import Control.Applicative
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Encoding (null_)
import Data.Default.Class
import qualified Data.Map as M
import Data.Semigroup (Last (..))
import Data.Text
import qualified Data.Vector.Unboxing as VU
import qualified DiPolysemy as Di
import GHC.Generics
import qualified Polysemy as P
import TextShow
import qualified TextShow.Generic as TSG

{- | Like whileM, but stateful effects are not preserved to mitigate memory leaks

 This means Polysemy.Error won't work to break the loop, etc.
 Instead, Error/Alternative will just result in the loop quitting.
-}
whileMFinalIO :: P.Sem r Bool -> P.Sem r ()
whileMFinalIO action = do
  go action
  where
    go action = do
      r <- action
      when r $ go_b action
    {-# INLINE go #-}
    go_b = go
    {-# NOINLINE go_b #-}

{- | Like untilJust, but stateful effects are not preserved to mitigate memory leaks

 This means Polysemy.Error won't work to break the loop, etc.
 Instead, Error/Alternative will just result in another loop.
-}
untilJustFinalIO :: P.Member (P.Final IO) r => P.Sem r (Maybe a) -> P.Sem r a
untilJustFinalIO action = do
  go action
  where
    go action = do
      r <- action
      case r of
        Just a ->
          pure a
        _ ->
          go_b action
    {-# INLINE go #-}
    go_b = go
    {-# NOINLINE go_b #-}

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= \case
    True -> m
    False -> pure ()

unlessM :: Monad m => m Bool -> m () -> m ()
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

newtype DefaultingMap k v = DefaultingMap {unDefaultingMap :: M.Map k v}

instance Default (DefaultingMap k v) where
  def = DefaultingMap M.empty

newtype AesonVector a = AesonVector {unAesonVector :: VU.Vector a}
  deriving (Show) via VU.Vector a

instance (FromJSON a, VU.Unboxable a) => FromJSON (AesonVector a) where
  parseJSON = (AesonVector . VU.fromList <$>) . parseJSON

instance (ToJSON a, VU.Unboxable a) => ToJSON (AesonVector a) where
  toJSON = toJSON . VU.toList . unAesonVector
  toEncoding = toEncoding . VU.toList . unAesonVector

instance (TextShow a, VU.Unboxable a) => TextShow (AesonVector a) where
  showb = showbList . VU.toList . unAesonVector

newtype CalamityFromStringShow a = CalamityFromStringShow {unCalamityFromStringShow :: a}
  deriving (FromJSON, ToJSON) via a
  deriving (TextShow) via FromStringShow a

{- | An alternative 'Maybe' type that allows us to distinguish between parsed
 json fields that were null, and fields that didn't exist.
-}
data MaybeNull a
  = WasNull
  | NotNull a
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric (MaybeNull a)

instance FromJSON a => FromJSON (MaybeNull a) where
  parseJSON Null = pure WasNull
  parseJSON x = NotNull <$> parseJSON x

instance ToJSON a => ToJSON (MaybeNull a) where
  toJSON WasNull = Null
  toJSON (NotNull x) = toJSON x

  toEncoding WasNull = null_
  toEncoding (NotNull x) = toEncoding x
