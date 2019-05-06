-- | The snowflake type
module Calamity.Types.Snowflake
    ( Snowflake(..)
    , HasID(..)
    , coerceSnowflake ) where

import           Control.Monad

import           Data.Aeson
import           Data.Data
import           Data.Generics.Product.Fields
import           Data.Text.Read
import qualified Data.Vector.Generic.Base     as V
import qualified Data.Vector.Generic.Mutable  as MV
import qualified Data.Vector.Unboxed          as U

-- Thanks sbrg
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs#L78
newtype Snowflake t = Snowflake
  { fromSnowflake :: Word64
  }
  deriving ( Generic, Show, Eq, Ord, Data, NFData, ToJSONKey )

instance Hashable (Snowflake t)

instance ToJSON (Snowflake t) where
  toJSON (Snowflake s) = String . show $ s

instance FromJSON (Snowflake t) where
  parseJSON = withText "Snowflake" $ \t -> do
    n <- case decimal t of
      Right (n, _) -> pure n
      Left e       -> fail e
    pure $ Snowflake n

coerceSnowflake :: Snowflake a -> Snowflake b
coerceSnowflake (Snowflake t) = Snowflake t

class HasID a where
  getID :: a -> Snowflake a

instance {-# OVERLAPPABLE #-}HasField "id" a a (Snowflake a) (Snowflake a) => HasID a where
  getID a = a ^. field @"id"

newtype instance U.MVector s (Snowflake t) = MV_Snowflake (U.MVector s Word64)

newtype instance U.Vector (Snowflake t) = V_Snowflake (U.Vector Word64)

instance MV.MVector U.MVector (Snowflake t) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Snowflake v) = MV.basicLength v

  basicUnsafeSlice i n (MV_Snowflake v) = MV_Snowflake $ MV.basicUnsafeSlice i n v

  basicOverlaps (MV_Snowflake v1) (MV_Snowflake v2) = MV.basicOverlaps v1 v2

  basicUnsafeNew n = MV_Snowflake `liftM` MV.basicUnsafeNew n

  basicInitialize (MV_Snowflake v) = MV.basicInitialize v

  basicUnsafeReplicate n x = MV_Snowflake `liftM` MV.basicUnsafeReplicate n (fromSnowflake x)

  basicUnsafeRead (MV_Snowflake v) i = Snowflake `liftM` MV.basicUnsafeRead v i

  basicUnsafeWrite (MV_Snowflake v) i x = MV.basicUnsafeWrite v i (fromSnowflake x)

  basicClear (MV_Snowflake v) = MV.basicClear v

  basicSet (MV_Snowflake v) x = MV.basicSet v (fromSnowflake x)

  basicUnsafeCopy (MV_Snowflake v1) (MV_Snowflake v2) = MV.basicUnsafeCopy v1 v2

  basicUnsafeMove (MV_Snowflake v1) (MV_Snowflake v2) = MV.basicUnsafeMove v1 v2

  basicUnsafeGrow (MV_Snowflake v) n = MV_Snowflake `liftM` MV.basicUnsafeGrow v n

instance V.Vector U.Vector (Snowflake t) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Snowflake v) = V_Snowflake `liftM` V.basicUnsafeFreeze v

  basicUnsafeThaw (V_Snowflake v) = MV_Snowflake `liftM` V.basicUnsafeThaw v

  basicLength (V_Snowflake v) = V.basicLength v

  basicUnsafeSlice i n (V_Snowflake v) = V_Snowflake $ V.basicUnsafeSlice i n v

  basicUnsafeIndexM (V_Snowflake v) i = Snowflake `liftM` V.basicUnsafeIndexM v i

  basicUnsafeCopy (MV_Snowflake mv) (V_Snowflake v) = V.basicUnsafeCopy mv v

  elemseq _ = seq

instance U.Unbox (Snowflake t)
