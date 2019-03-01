-- | Module for custom instance of Data.HashMap.Lazy that decodes from any list of objects that have an id field

module YAHDL.Types.SnowflakeMap where

import           Control.Lens.At
import           Control.Lens.Wrapped
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , withArray
                                                )
import           Data.Data
import           Data.HashMap.Lazy              ( HashMap )
import qualified Data.HashMap.Lazy             as LH
import           GHC.Exts                       ( IsList )
import           Prelude                 hiding ( over )
import           Unsafe.Coerce

import           YAHDL.Types.Snowflake

-- TODO: From/ToJSON instance
-- TODO: has ID constraints, etc

newtype SnowflakeMap a = SnowflakeMap
  { unSnowflakeMap :: HashMap (Snowflake a) a
  } deriving (Generic, IsList, Eq, Data, Ord, Show, Semigroup, Monoid)

-- instance At (SnowflakeMap a) where
--   at k f m = at (unSnowflakeMap k) f m

instance Functor SnowflakeMap where
  fmap f = SnowflakeMap . coerceSnowflakeMap . fmap f . unSnowflakeMap

instance Foldable SnowflakeMap where
  foldr f b = Prelude.foldr f b . unSnowflakeMap

instance Traversable SnowflakeMap where
  traverse f = fmap (SnowflakeMap . coerceSnowflakeMap) . traverse f . unSnowflakeMap

deriving instance NFData a => NFData (SnowflakeMap a)
deriving instance Hashable a => Hashable (SnowflakeMap a)

instance Wrapped (SnowflakeMap a) where
  type Unwrapped (SnowflakeMap a) = HashMap (Snowflake a) a
  _Wrapped' = iso unSnowflakeMap SnowflakeMap

type instance (Index (SnowflakeMap a)) = Snowflake a
type instance (IxValue (SnowflakeMap a)) = a

instance SnowflakeMap a ~ t => Rewrapped (SnowflakeMap b) a

instance Ixed (SnowflakeMap a) where
  ix i = _Wrapped . ix i

instance At (SnowflakeMap a) where
  at i = _Wrapped . at i

over :: (HashMap (Snowflake a) a -> HashMap (Snowflake b) b) -> SnowflakeMap a -> SnowflakeMap b
over f =  SnowflakeMap . f . unSnowflakeMap
{-# INLINABLE over #-}

-- I guess I could just do this by unwrapping everything and using coerceSnowflake
coerceSnowflakeMap :: HashMap (Snowflake a) v -> HashMap (Snowflake b) v
coerceSnowflakeMap = unsafeCoerce
{-# INLINABLE coerceSnowflakeMap #-}

empty :: SnowflakeMap a
empty = SnowflakeMap LH.empty
{-# INLINABLE empty #-}

singleton :: HasID a => a -> SnowflakeMap a
singleton v = SnowflakeMap $ LH.singleton (getID v) v
{-# INLINABLE singleton #-}

null :: SnowflakeMap a -> Bool
null = LH.null . unSnowflakeMap
{-# INLINABLE null #-}

size :: SnowflakeMap a -> Int
size = LH.size . unSnowflakeMap
{-# INLINABLE size #-}

member :: Snowflake a -> SnowflakeMap a -> Bool
member k = LH.member k . unSnowflakeMap
{-# INLINABLE member #-}

lookup :: Snowflake a -> SnowflakeMap a -> Maybe a
lookup k = LH.lookup k . unSnowflakeMap
{-# INLINABLE lookup #-}

lookupDefault :: a -> Snowflake a -> SnowflakeMap a -> a
lookupDefault d k = LH.lookupDefault d k . unSnowflakeMap
{-# INLINABLE lookupDefault #-}

(!) :: SnowflakeMap a -> Snowflake a -> a
(!) m k = unSnowflakeMap m LH.! k
{-# INLINABLE (!) #-}

insert :: HasID a => a -> SnowflakeMap a -> SnowflakeMap a
insert v = over $ LH.insert (getID v) v
{-# INLINABLE insert #-}

insertWith :: HasID a => (a -> a -> a) -> a -> SnowflakeMap a -> SnowflakeMap a
insertWith f v = over $ LH.insertWith f (getID v) v
{-# INLINABLE insertWith #-}

delete :: Snowflake a -> SnowflakeMap a -> SnowflakeMap a
delete k = over $ LH.delete k
{-# INLINABLE delete #-}

adjust :: (a -> a) -> Snowflake a -> SnowflakeMap a -> SnowflakeMap a
adjust f k = over $ LH.adjust f k
{-# INLINABLE adjust #-}

update :: (a -> Maybe a) -> Snowflake a -> SnowflakeMap a -> SnowflakeMap a
update f k = over $ LH.update f k
{-# INLINABLE update #-}

alter :: (Maybe a -> Maybe a) -> Snowflake a -> SnowflakeMap a -> SnowflakeMap a
alter f k = over $ LH.alter f k
{-# INLINABLE alter #-}

union :: SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
union m m' = SnowflakeMap $ LH.union (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE union #-}

unionWith :: (a -> a -> a) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
unionWith f m m' = SnowflakeMap $ LH.unionWith f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE unionWith #-}

unionWithKey :: (Snowflake a -> a -> a -> a) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
unionWithKey f m m' = SnowflakeMap $ LH.unionWithKey f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE unionWithKey #-}

unions :: [SnowflakeMap a] -> SnowflakeMap a
unions = SnowflakeMap . LH.unions . Prelude.map unSnowflakeMap
{-# INLINABLE unions #-}

map :: (a1 -> a2) -> SnowflakeMap a1 -> SnowflakeMap a2
map f = over $ coerceSnowflakeMap . LH.map f
{-# INLINABLE map #-}

mapWithKey :: (Snowflake a1 -> a1 -> a2) -> SnowflakeMap a1 -> SnowflakeMap a2
mapWithKey f = over $ coerceSnowflakeMap . LH.mapWithKey f
{-# INLINABLE mapWithKey #-}

traverseWithKey :: Applicative f => (Snowflake a1 -> a1 -> f a2) -> SnowflakeMap a1 -> f (SnowflakeMap a2)
traverseWithKey f = fmap (SnowflakeMap . coerceSnowflakeMap) . LH.traverseWithKey f . unSnowflakeMap
{-# INLINABLE traverseWithKey #-}

difference :: SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
difference m m' = SnowflakeMap $ LH.difference (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE difference #-}

differenceWith :: (a -> a -> Maybe a) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
differenceWith f m m' = SnowflakeMap $ LH.differenceWith f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE differenceWith #-}

intersection :: SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap a
intersection m m' = SnowflakeMap $ LH.intersection (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE intersection #-}

intersectionWith :: (a -> a -> b) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap b
intersectionWith f m m' = SnowflakeMap . coerceSnowflakeMap $ LH.intersectionWith f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE intersectionWith #-}

intersectionWithKey :: (Snowflake a -> a -> a -> b) -> SnowflakeMap a -> SnowflakeMap a -> SnowflakeMap b
intersectionWithKey f m m' = SnowflakeMap . coerceSnowflakeMap $ LH.intersectionWithKey f (unSnowflakeMap m) (unSnowflakeMap m')
{-# INLINABLE intersectionWithKey #-}

foldl' :: (a -> b -> a) -> a -> SnowflakeMap b -> a
foldl' f s m = LH.foldl' f s $ unSnowflakeMap m
{-# INLINABLE foldl' #-}

foldlWithKey' :: (a -> Snowflake b -> b -> a) -> a -> SnowflakeMap b -> a
foldlWithKey' f s m = LH.foldlWithKey' f s $ unSnowflakeMap m
{-# INLINABLE foldlWithKey' #-}

foldr :: (b -> a -> a) -> a -> SnowflakeMap b -> a
foldr f s m = LH.foldr f s $ unSnowflakeMap m
{-# INLINABLE foldr #-}

foldrWithKey :: (Snowflake b -> b -> a -> a) -> a -> SnowflakeMap b -> a
foldrWithKey f s m = LH.foldrWithKey f s $ unSnowflakeMap m
{-# INLINABLE foldrWithKey #-}

filter :: (a -> Bool) -> SnowflakeMap a -> SnowflakeMap a
filter f = over $ LH.filter f
{-# INLINABLE filter #-}

filterWithKey :: (Snowflake a -> a -> Bool) -> SnowflakeMap a -> SnowflakeMap a
filterWithKey f = over $ LH.filterWithKey f
{-# INLINABLE filterWithKey #-}

mapMaybe :: (a -> Maybe b) -> SnowflakeMap a -> SnowflakeMap b
mapMaybe f = over $ coerceSnowflakeMap . LH.mapMaybe f
{-# INLINABLE mapMaybe #-}

mapMaybeWithKey :: (Snowflake a -> a -> Maybe b) -> SnowflakeMap a -> SnowflakeMap b
mapMaybeWithKey f = over $ coerceSnowflakeMap . LH.mapMaybeWithKey f
{-# INLINABLE mapMaybeWithKey #-}

keys :: SnowflakeMap a -> [Snowflake a]
keys = LH.keys . unSnowflakeMap
{-# INLINABLE keys #-}

elems :: SnowflakeMap a -> [a]
elems = LH.elems . unSnowflakeMap
{-# INLINABLE elems #-}

toList :: SnowflakeMap a -> [(Snowflake a, a)]
toList = LH.toList . unSnowflakeMap
{-# INLINABLE toList #-}

fromList :: HasID a => [a] -> SnowflakeMap a
fromList = SnowflakeMap . LH.fromList . Prelude.map (\v -> (getID v, v))
{-# INLINABLE fromList #-}

fromListWith :: HasID a => (a -> a -> a) -> [a] -> SnowflakeMap a
fromListWith f = SnowflakeMap . LH.fromListWith f . Prelude.map (\v -> (getID v, v))
{-# INLINABLE fromListWith #-}

instance (FromJSON a, HasID a) => FromJSON (SnowflakeMap a) where
  parseJSON = withArray "SnowflakeMap" $ \l -> do
    parsed <- mapM parseJSON l
    pure . YAHDL.Types.SnowflakeMap.fromList . Prelude.toList $ parsed

instance ToJSON a => ToJSON (SnowflakeMap a) where
  toEncoding = toEncoding . elems
