{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | A thing for storing the last N things with IDs
module Calamity.Internal.BoundedStore (
  BoundedStore,
  empty,
  addItem,
  getItem,
  dropItem,
) where

import Calamity.Internal.Utils (unlessM, whenM)
import Calamity.Types.Snowflake (HasID (getID), HasID', Snowflake)
import Control.Monad (when)
import Control.Monad.State.Lazy (execState)
import Data.Default.Class (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Deque.Lazy (Deque)
import Deque.Lazy qualified as DQ
import Optics
import Optics.State.Operators ((%=), (.=))

data BoundedStore a = BoundedStore
  { itemQueue :: Deque (Snowflake a)
  , items :: HashMap (Snowflake a) a
  , limit :: Int
  , size :: Int
  }
  deriving (Show)

$(makeFieldLabelsNoPrefix ''BoundedStore)

instance Foldable BoundedStore where
  foldr f i = foldr f i . H.elems . items

instance Default (BoundedStore a) where
  def = BoundedStore mempty mempty 1000 0

empty :: Int -> BoundedStore a
empty limit = BoundedStore mempty mempty limit 0

type instance Index (BoundedStore a) = Snowflake a

type instance IxValue (BoundedStore a) = a

instance (HasID' a) => Ixed (BoundedStore a)

instance (HasID' a) => At (BoundedStore a) where
  at k = lensVL $ \f m ->
    let mv = getItem k m
     in f mv <&> \case
          Nothing -> maybe m (const (dropItem k m)) mv
          Just v -> addItem v m
  {-# INLINE at #-}

addItem :: (HasID' a) => a -> BoundedStore a -> BoundedStore a
addItem m = execState $ do
  unlessM (H.member (getID m) <$> use #items) $ do
    #itemQueue %= DQ.cons (getID m)
    #size %= succ

  size <- use #size
  limit <- use #limit

  when (size > limit) $ do
    q <- use #itemQueue
    let Just (rid, q') = DQ.unsnoc q
    #itemQueue .= q'
    #items %= sans rid
    #size %= pred

  #items %= H.insert (getID m) m
{-# INLINE addItem #-}

getItem :: Snowflake a -> BoundedStore a -> Maybe a
getItem id s = H.lookup id (s ^. #items)
{-# INLINE getItem #-}

dropItem :: Snowflake a -> BoundedStore a -> BoundedStore a
dropItem id = execState $ do
  whenM (H.member id <$> use #items) $ do
    #size %= pred

  #itemQueue %= DQ.filter (/= id)
  #items %= H.delete id
{-# INLINE dropItem #-}
