-- | RefCounted SnowflakeMap
module Calamity.Types.RefCountedSnowflakeMap where

import           Calamity.Types.Snowflake

-- import           Control.Lens.At
-- import           Control.Lens.Wrapped
import           Data.Aeson               ( FromJSON(..), ToJSON(..), withArray )
import           Data.Data
import           Data.HashMap.Lazy        ( HashMap )
import qualified Data.HashMap.Lazy        as LH
import qualified Data.List                as L

import           GHC.Exts                 ( IsList )

import           Prelude                  hiding ( over )

import           Unsafe.Coerce

newtype RefCountedSnowflakeMap a = RefCountedSnowflakeMap
  { unRefCountedSnowflakeMap :: HashMap (Snowflake a) (a, Int)
  }
  deriving ( Generic, Eq, Data, Ord, Show )
  deriving newtype ( IsList, Semigroup, Monoid )
  deriving anyclass ( NFData, Hashable )

   -- instance At (RefCountedSnowflakeMap a) where
   --   at k f m = at (unRefCountedSnowflakeMap k) f m
instance Functor RefCountedSnowflakeMap where
  fmap f = RefCountedSnowflakeMap . coerceRefCountedSnowflakeMap . fmap (first f) . unRefCountedSnowflakeMap

instance Foldable RefCountedSnowflakeMap where
  foldr f b = Prelude.foldr (f . fst) b . unRefCountedSnowflakeMap



-- instance Traversable RefCountedSnowflakeMap where
--   traverse f m = let (vals, counts) = unzip . unRefCountedSnowflakeMap $ m
--                      vals' = traverse f vals
--                      zipped = zipR (vals', counts)
--                  in
--   fmap (RefCountedSnowflakeMap . coerceRefCountedSnowflakeMap) . traverse f . unRefCountedSnowflakeMap

instance Traversable RefCountedSnowflakeMap where
  traverse f = fmap (RefCountedSnowflakeMap . coerceRefCountedSnowflakeMap) . traverse (\(v, c) -> (, c) <$> f v)
    . unRefCountedSnowflakeMap

   -- deriving instance NFData a => NFData (RefCountedSnowflakeMap a)
   -- deriving instance Hashable a => Hashable (RefCountedSnowflakeMap a)
-- instance Wrapped (RefCountedSnowflakeMap a) where
--   type Unwrapped (RefCountedSnowflakeMap a) = HashMap (Snowflake a) (a, Int)

--   _Wrapped' = iso unRefCountedSnowflakeMap RefCountedSnowflakeMap

-- type instance (Index (RefCountedSnowflakeMap a)) = Snowflake a

-- type instance (IxValue (RefCountedSnowflakeMap a)) = a

-- instance RefCountedSnowflakeMap a ~ t => Rewrapped (RefCountedSnowflakeMap b) a

-- -- instance Ixed (RefCountedSnowflakeMap a) where
-- --   ix i = _Wrapped . ix i . _Just . _1

-- instance At (RefCountedSnowflakeMap a) where
--   at i = _Wrapped . at i . _Just -- . _1

over :: (HashMap (Snowflake a) (a, Int) -> HashMap (Snowflake b) (b, Int)) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap b
over f = RefCountedSnowflakeMap . f . unRefCountedSnowflakeMap

{-# INLINABLE over #-}
-- I guess I could just do this by unwrapping everything and using coerceSnowflake
coerceRefCountedSnowflakeMap :: HashMap (Snowflake a) (v, Int) -> HashMap (Snowflake b) (v, Int)
coerceRefCountedSnowflakeMap = unsafeCoerce

{-# INLINABLE coerceRefCountedSnowflakeMap #-}
empty :: RefCountedSnowflakeMap a
empty = RefCountedSnowflakeMap LH.empty

{-# INLINABLE empty #-}
singleton :: HasID a => a -> RefCountedSnowflakeMap a
singleton v = RefCountedSnowflakeMap $ LH.singleton (coerceSnowflake $ getID v) (v, 1)

{-# INLINABLE singleton #-}
null :: RefCountedSnowflakeMap a -> Bool
null = LH.null . unRefCountedSnowflakeMap

{-# INLINABLE null #-}
size :: RefCountedSnowflakeMap a -> Int
size = LH.size . unRefCountedSnowflakeMap

{-# INLINABLE size #-}
member :: Snowflake a -> RefCountedSnowflakeMap a -> Bool
member k = LH.member k . unRefCountedSnowflakeMap

{-# INLINABLE member #-}
lookup :: Snowflake a -> RefCountedSnowflakeMap a -> Maybe a
lookup k = fmap fst . LH.lookup k . unRefCountedSnowflakeMap

{-# INLINABLE lookup #-}
lookupDefault :: a -> Snowflake a -> RefCountedSnowflakeMap a -> a
lookupDefault d k = fst . LH.lookupDefault (d, 1) k . unRefCountedSnowflakeMap

{-# INLINABLE lookupDefault #-}
(!) :: RefCountedSnowflakeMap a -> Snowflake a -> a
(!) m k = fst $ unRefCountedSnowflakeMap m LH.! k

{-# INLINABLE (!) #-}
insert :: HasID a => a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
insert v = over $ LH.insertWith (\(_, l) (n, r) -> (n, l + r)) (coerceSnowflake $ getID v) (v, 1)

{-# INLINABLE insert #-}
insertWith :: HasID a => (a -> a -> a) -> a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
insertWith f v = over $ LH.insertWith (\(o, l) (n, r) -> (f o n, l + r)) (coerceSnowflake $ getID v) (v, 1)

{-# INLINABLE insertWith #-}
delete :: Snowflake a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
delete k = over $ LH.alter (\case
                              Just (_, 1) -> Nothing
                              Just (v, c) -> Just (v, c - 1)
                              _           -> Nothing) k

{-# INLINABLE delete #-}
adjust :: (a -> a) -> Snowflake a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
adjust f k = over $ LH.adjust (first f) k

{-# INLINABLE adjust #-}
update :: (a -> Maybe a) -> Snowflake a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
update f k = over $ LH.update (\(v, c) -> (, c) <$> f v) k

{-# INLINABLE update #-}
alter :: (Maybe a -> Maybe a) -> Snowflake a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
alter f k = over $ LH.alter
  (\case
     Just (v, c) -> case f (Just v) of
       Just v' -> Just (v', c)
       Nothing -> if c == 1
                  then Nothing
                  else Just (v, c - 1)
     Nothing     -> case f Nothing of
       Just v  -> Just (v, 1)
       Nothing -> Nothing) k

{-# INLINABLE alter #-}
union :: RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
union m m' = RefCountedSnowflakeMap $ LH.unionWith (\(v, l) (_, r) -> (v, l + r)) (unRefCountedSnowflakeMap m)
  (unRefCountedSnowflakeMap m')

{-# INLINABLE union #-}
unionWith :: (a -> a -> a) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
unionWith f m m' = RefCountedSnowflakeMap $ LH.unionWith (\(v, l) (v', r) -> (f v v', l + r))
  (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')

{-# INLINABLE unionWith #-}
unionWithKey
  :: (Snowflake a -> a -> a -> a) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
unionWithKey f m m' = RefCountedSnowflakeMap $ LH.unionWithKey (\k (v, l) (v', r) -> (f k v v', l + r))
  (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')

{-# INLINABLE unionWithKey #-}
unions :: [RefCountedSnowflakeMap a] -> RefCountedSnowflakeMap a
unions = L.foldl' union Calamity.Types.RefCountedSnowflakeMap.empty

{-# INLINABLE unions #-}
map :: (a1 -> a2) -> RefCountedSnowflakeMap a1 -> RefCountedSnowflakeMap a2
map f = over $ coerceRefCountedSnowflakeMap . LH.map (first f)

{-# INLINABLE map #-}
mapWithKey :: (Snowflake a1 -> a1 -> a2) -> RefCountedSnowflakeMap a1 -> RefCountedSnowflakeMap a2
mapWithKey f = over $ coerceRefCountedSnowflakeMap . LH.mapWithKey (first . f)

{-# INLINABLE mapWithKey #-}
traverseWithKey
  :: Applicative f => (Snowflake a1 -> a1 -> f a2) -> RefCountedSnowflakeMap a1 -> f (RefCountedSnowflakeMap a2)
traverseWithKey f = fmap (RefCountedSnowflakeMap . coerceRefCountedSnowflakeMap) . LH.traverseWithKey
  (\k (v, c) -> (, c) <$> f k v) . unRefCountedSnowflakeMap

{-# INLINABLE traverseWithKey #-}
difference :: RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
difference m m' = RefCountedSnowflakeMap $ LH.differenceWith
  (\(v, l) (_, r) -> if l - r > 0
                     then Just (v, l - r)
                     else Nothing) (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')

{-# INLINABLE difference #-}
-- differenceWith :: (a -> a -> Maybe a) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
-- differenceWith f m m' = RefCountedSnowflakeMap $ LH.differenceWith f (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')
-- {-# INLINABLE differenceWith #-}
intersection :: RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
intersection m m' = RefCountedSnowflakeMap $ LH.intersectionWith (\(v, l) (_, r) -> (v, l + r))
  (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')

{-# INLINABLE intersection #-}
intersectionWith :: (a -> a -> b) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap b
intersectionWith f m m' = RefCountedSnowflakeMap . coerceRefCountedSnowflakeMap $ LH.intersectionWith
  (\(v, l) (v', r) -> (f v v', l + r)) (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')

-- {-# INLINABLE intersectionWith #-}
-- intersectionWithKey
--   :: (Snowflake a -> a -> a -> b) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap b
-- intersectionWithKey f m m' = RefCountedSnowflakeMap . coerceRefCountedSnowflakeMap $ LH.intersectionWithKey f
--   (unRefCountedSnowflakeMap m) (unRefCountedSnowflakeMap m')
-- {-# INLINABLE intersectionWithKey #-}
foldl' :: (a -> b -> a) -> a -> RefCountedSnowflakeMap b -> a
foldl' f s m = LH.foldl' (\a (v, _) -> f a v) s $ unRefCountedSnowflakeMap m

{-# INLINABLE foldl' #-}
foldlWithKey' :: (a -> Snowflake b -> b -> a) -> a -> RefCountedSnowflakeMap b -> a
foldlWithKey' f s m = LH.foldlWithKey' (\a k (v, _) -> f a k v) s $ unRefCountedSnowflakeMap m

{-# INLINABLE foldlWithKey' #-}
foldr :: (b -> a -> a) -> a -> RefCountedSnowflakeMap b -> a
foldr f s m = LH.foldr (f . fst) s $ unRefCountedSnowflakeMap m

{-# INLINABLE foldr #-}
foldrWithKey :: (Snowflake b -> b -> a -> a) -> a -> RefCountedSnowflakeMap b -> a
foldrWithKey f s m = LH.foldrWithKey (\k (v, _) -> f k v) s $ unRefCountedSnowflakeMap m

{-# INLINABLE foldrWithKey #-}
filter :: (a -> Bool) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
filter f = over $ LH.filter (f . fst)

{-# INLINABLE filter #-}
filterWithKey :: (Snowflake a -> a -> Bool) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap a
filterWithKey f = over $ LH.filterWithKey (\k (v, _) -> f k v)

{-# INLINABLE filterWithKey #-}
mapMaybe :: (a -> Maybe b) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap b
mapMaybe f = over $ coerceRefCountedSnowflakeMap . LH.mapMaybe (\(v, c) -> (, c) <$> f v)

{-# INLINABLE mapMaybe #-}
mapMaybeWithKey :: (Snowflake a -> a -> Maybe b) -> RefCountedSnowflakeMap a -> RefCountedSnowflakeMap b
mapMaybeWithKey f = over $ coerceRefCountedSnowflakeMap . LH.mapMaybeWithKey (\k (v, c) -> (, c) <$> f k v)

{-# INLINABLE mapMaybeWithKey #-}
keys :: RefCountedSnowflakeMap a -> [Snowflake a]
keys = LH.keys . unRefCountedSnowflakeMap

{-# INLINABLE keys #-}
elems :: RefCountedSnowflakeMap a -> [a]
elems = Prelude.map fst . LH.elems . unRefCountedSnowflakeMap

{-# INLINABLE elems #-}
toList :: RefCountedSnowflakeMap a -> [(Snowflake a, a)]
toList = Prelude.map (\(k, (v, _)) -> (k, v)) . LH.toList . unRefCountedSnowflakeMap

{-# INLINABLE toList #-}
fromList :: HasID a => [a] -> RefCountedSnowflakeMap a
fromList = RefCountedSnowflakeMap . LH.fromList . Prelude.map (\v -> (coerceSnowflake $ getID v, (v, 1)))
{-# INLINABLE fromList #-}

-- fromListWith :: HasID a => (a -> a -> a) -> [a] -> RefCountedSnowflakeMap a
-- fromListWith f = RefCountedSnowflakeMap . LH.fromListWith f . Prelude.map (\v -> (getID v, (v, 1)))
-- {-# INLINABLE fromListWith #-}

instance (FromJSON a, HasID a) => FromJSON (RefCountedSnowflakeMap a) where
  parseJSON = withArray "RefCountedSnowflakeMap" $ \l -> do
    parsed <- traverse parseJSON l
    pure . Calamity.Types.RefCountedSnowflakeMap.fromList . Prelude.toList $ parsed

instance ToJSON a => ToJSON (RefCountedSnowflakeMap a) where
  toEncoding = toEncoding . elems
