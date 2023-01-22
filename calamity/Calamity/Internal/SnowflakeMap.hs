-- | Module for custom instance of Data.HashMap.Strict that decodes from any list of objects that have an id field
module Calamity.Internal.SnowflakeMap where

import Calamity.Internal.Utils ()
import Calamity.Types.Snowflake
import Data.Aeson (FromJSON (..), ToJSON (..), withArray)
import Data.Data
import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as SH
import Data.Hashable
import GHC.Exts (IsList)
import Optics
import TextShow
import Unsafe.Coerce

newtype SnowflakeMap a = SnowflakeMap
  { unSnowflakeMap :: HashMap (Snowflake a) a
  }
  deriving stock (Eq, Data, Ord, Show)
  deriving (TextShow) via FromStringShow (SnowflakeMap a)
  deriving newtype (IsList, Semigroup, Monoid)
  deriving newtype (Hashable)

-- instance At (SnowflakeMap a) where
--   at k f m = at (unSnowflakeMap k) f m

instance Functor SnowflakeMap where
  fmap f = SnowflakeMap . coerceSnowflakeMap . fmap f . unSnowflakeMap

instance Foldable SnowflakeMap where
  foldr f b = Prelude.foldr f b . unSnowflakeMap

instance Traversable SnowflakeMap where
  traverse f = fmap (SnowflakeMap . coerceSnowflakeMap) . traverse f . unSnowflakeMap

type instance Index (SnowflakeMap a) = Snowflake a

type instance IxValue (SnowflakeMap a) = a

_SnowflakeMap ::
  ( Iso
      (SnowflakeMap a)
      (SnowflakeMap a1)
      (HashMap (Snowflake a) a)
      (HashMap (Snowflake a1) a1)
  )
_SnowflakeMap = iso unSnowflakeMap SnowflakeMap

instance Ixed (SnowflakeMap a) where
  ix i = _SnowflakeMap % ix i
  {-# INLINE ix #-}

instance At (SnowflakeMap a) where
  at i = _SnowflakeMap % at i
  {-# INLINE at #-}

overSM :: (HashMap (Snowflake a) a -> HashMap (Snowflake b) b) -> SnowflakeMap a -> SnowflakeMap b
overSM f = SnowflakeMap . f . unSnowflakeMap
{-# INLINEABLE overSM #-}

-- SAFETY: 'Snowflake' always uses the underlying hash function (Word64)
coerceSnowflakeMap :: HashMap (Snowflake a) v -> HashMap (Snowflake b) v
coerceSnowflakeMap = unsafeCoerce
{-# INLINEABLE coerceSnowflakeMap #-}

empty :: SnowflakeMap a
empty = SnowflakeMap SH.empty
{-# INLINEABLE empty #-}

singleton :: HasID' a => a -> SnowflakeMap a
singleton v = SnowflakeMap $ SH.singleton (getID v) v
{-# INLINEABLE singleton #-}

null :: SnowflakeMap a -> Bool
null = SH.null . unSnowflakeMap
{-# INLINEABLE null #-}

size :: SnowflakeMap a -> Int
size = SH.size . unSnowflakeMap
{-# INLINEABLE size #-}

member :: Snowflake a -> SnowflakeMap a -> Bool
member k = SH.member k . unSnowflakeMap
{-# INLINEABLE member #-}

lookup :: Snowflake a -> SnowflakeMap a -> Maybe a
lookup k = SH.lookup k . unSnowflakeMap
{-# INLINEABLE lookup #-}

lookupDefault :: a -> Snowflake a -> SnowflakeMap a -> a
lookupDefault d k = SH.lookupDefault d k . unSnowflakeMap
{-# INLINEABLE lookupDefault #-}

(!) :: SnowflakeMap a -> Snowflake a -> a
(!) m k = unSnowflakeMap m SH.! k
{-# INLINEABLE (!) #-}

infixl 9 !

insert :: HasID' a => a -> SnowflakeMap a -> SnowflakeMap a
insert v = overSM $ SH.insert (getID v) v
{-# INLINEABLE insert #-}

insertWith :: HasID' a => (a -> a -> a) -> a -> SnowflakeMap a -> SnowflakeMap a
insertWith f v = overSM $ SH.insertWith f (getID v) v
{-# INLINEABLE insertWith #-}

delete :: Snowflake a -> SnowflakeMap a -> SnowflakeMap a
delete k = overSM $ SH.delete k
{-# INLINEABLE delete #-}

adjust :: (a -> a) -> Snowflake a -> SnowflakeMap a -> SnowflakeMap a
adjust f k = overSM $ SH.adjust f k
{-# INLINEABLE adjust #-}

update :: (a -> Maybe a) -> Snowflake a -> SnowflakeMap a -> SnowflakeMap a
update f k = overSM $ SH.update f k
{-# INLINEABLE update #-}

alter :: (Maybe a -> Maybe a) -> Snowflake a -> SnowflakeMap a -> SnowflakeMap a
alter f k = overSM $ SH.alter f k
{-# INLINEABLE alter #-}

union :: SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
union m m' = SnowflakeMap $ SH.union (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE union #-}

unionWith :: (a -> a -> a) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
unionWith f m m' = SnowflakeMap $ SH.unionWith f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE unionWith #-}

unionWithKey :: (Snowflake a -> a -> a -> a) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
unionWithKey f m m' = SnowflakeMap $ SH.unionWithKey f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE unionWithKey #-}

unions :: [SnowflakeMap a] -> SnowflakeMap a
unions = SnowflakeMap . SH.unions . Prelude.map unSnowflakeMap
{-# INLINEABLE unions #-}

map :: (a1 -> a2) -> SnowflakeMap a1 -> SnowflakeMap a2
map f = overSM $ coerceSnowflakeMap . SH.map f
{-# INLINEABLE map #-}

mapWithKey :: (Snowflake a1 -> a1 -> a2) -> SnowflakeMap a1 -> SnowflakeMap a2
mapWithKey f = overSM $ coerceSnowflakeMap . SH.mapWithKey f
{-# INLINEABLE mapWithKey #-}

traverseWithKey :: Applicative f => (Snowflake a1 -> a1 -> f a2) -> SnowflakeMap a1 -> f (SnowflakeMap a2)
traverseWithKey f = fmap (SnowflakeMap . coerceSnowflakeMap) . SH.traverseWithKey f . unSnowflakeMap
{-# INLINEABLE traverseWithKey #-}

difference :: SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
difference m m' = SnowflakeMap $ SH.difference (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE difference #-}

differenceWith :: (a -> a -> Maybe a) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
differenceWith f m m' = SnowflakeMap $ SH.differenceWith f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE differenceWith #-}

intersection :: SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
intersection m m' = SnowflakeMap $ SH.intersection (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE intersection #-}

intersectionWith :: (a -> a -> b) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap b
intersectionWith f m m' = SnowflakeMap . coerceSnowflakeMap $ SH.intersectionWith f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE intersectionWith #-}

intersectionWithKey :: (Snowflake a -> a -> a -> b) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap b
intersectionWithKey f m m' = SnowflakeMap . coerceSnowflakeMap $ SH.intersectionWithKey f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINEABLE intersectionWithKey #-}

foldl' :: (a -> b -> a) -> a -> SnowflakeMap b -> a
foldl' f s m = SH.foldl' f s $ unSnowflakeMap m
{-# INLINEABLE foldl' #-}

foldlWithKey' :: (a -> Snowflake b -> b -> a) -> a -> SnowflakeMap b -> a
foldlWithKey' f s m = SH.foldlWithKey' f s $ unSnowflakeMap m
{-# INLINEABLE foldlWithKey' #-}

foldr :: (b -> a -> a) -> a -> SnowflakeMap b -> a
foldr f s m = SH.foldr f s $ unSnowflakeMap m
{-# INLINEABLE foldr #-}

foldrWithKey :: (Snowflake b -> b -> a -> a) -> a -> SnowflakeMap b -> a
foldrWithKey f s m = SH.foldrWithKey f s $ unSnowflakeMap m
{-# INLINEABLE foldrWithKey #-}

filter :: (a -> Bool) -> SnowflakeMap a -> SnowflakeMap a
filter f = overSM $ SH.filter f
{-# INLINEABLE filter #-}

filterWithKey :: (Snowflake a -> a -> Bool) -> SnowflakeMap a -> SnowflakeMap a
filterWithKey f = overSM $ SH.filterWithKey f
{-# INLINEABLE filterWithKey #-}

mapMaybe :: (a -> Maybe b) -> SnowflakeMap a -> SnowflakeMap b
mapMaybe f = overSM $ coerceSnowflakeMap . SH.mapMaybe f
{-# INLINEABLE mapMaybe #-}

mapMaybeWithKey :: (Snowflake a -> a -> Maybe b) -> SnowflakeMap a -> SnowflakeMap b
mapMaybeWithKey f = overSM $ coerceSnowflakeMap . SH.mapMaybeWithKey f
{-# INLINEABLE mapMaybeWithKey #-}

keys :: SnowflakeMap a -> [Snowflake a]
keys = SH.keys . unSnowflakeMap
{-# INLINEABLE keys #-}

elems :: SnowflakeMap a -> [a]
elems = SH.elems . unSnowflakeMap
{-# INLINEABLE elems #-}

toList :: SnowflakeMap a -> [(Snowflake a, a)]
toList = SH.toList . unSnowflakeMap
{-# INLINEABLE toList #-}

fromList :: HasID' a => [a] -> SnowflakeMap a
fromList = SnowflakeMap . SH.fromList . Prelude.map (\v -> (getID v, v))
{-# INLINEABLE fromList #-}

fromListWith :: HasID' a => (a -> a -> a) -> [a] -> SnowflakeMap a
fromListWith f = SnowflakeMap . SH.fromListWith f . Prelude.map (\v -> (getID v, v))
{-# INLINEABLE fromListWith #-}

instance (FromJSON a, HasID' a) => FromJSON (SnowflakeMap a) where
  parseJSON = withArray "SnowflakeMap" $ \l -> do
    parsed <- traverse parseJSON l
    pure . Calamity.Internal.SnowflakeMap.fromList . F.toList $ parsed

instance ToJSON a => ToJSON (SnowflakeMap a) where
  toJSON = toJSON . elems
  toEncoding = toEncoding . elems
