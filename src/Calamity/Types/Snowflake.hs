-- | The snowflake type
module Calamity.Types.Snowflake
    ( Snowflake(..)
    , HasID(..)
    , type HasID'
    , HasIDField(..)
    , HasIDFieldCoerce(..)
    , type HasIDFieldCoerce'
    , coerceSnowflake ) where

import           Control.DeepSeq
import           Control.Lens

import           Data.Aeson
import           Data.Data
import           Data.Generics.Product.Fields
import           Data.Hashable
import           Data.Kind
import           Data.Text.Read
import qualified Data.Vector.Unboxing         as U
import           Data.Word
import           Data.Bits

import           GHC.Generics

import           TextShow

-- Thanks sbrg
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs#L78
newtype Snowflake (t :: Type) = Snowflake
  { fromSnowflake :: Word64
  }
  deriving ( Generic, Eq, Ord, Data )
  deriving ( Show, TextShow ) via Word64
  deriving newtype ( NFData, ToJSONKey, U.Unboxable )

-- I'm pretty sure that Word64's hash just being 'fromIntegral' is a bad idea when
-- attempting to use it in a hashmap, so swizzle the bits a bit to give a good
-- distribution of bits
instance Hashable (Snowflake t) where
  hashWithSalt salt (Snowflake a) =
    let initial = fromIntegral @_ @Word64 $ hashWithSalt salt a
        round1 = ((initial `shiftR` 30) `xor` initial) * 0xbf58476d1ce4e5b9
        round2 = ((round1 `shiftR` 27) `xor` round1) * 0xbf58476d1ce4e5b9
        round3 = ((round2 `shiftR` 31) `xor` round2)
     in fromIntegral @_ @Int round3

instance ToJSON (Snowflake t) where
  toJSON (Snowflake s) = String . showt $ s

instance FromJSON (Snowflake t) where
  parseJSON = withText "Snowflake" $ \t -> do
    n <- case decimal t of
      Right (n, _) -> pure n
      Left e       -> fail e
    pure $ Snowflake n

coerceSnowflake :: Snowflake a -> Snowflake b
coerceSnowflake (Snowflake t) = Snowflake t

-- | A typeclass for types that contain snowflakes of type `b`
class HasID b a where

  -- | Retrieve the ID from the type
  getID :: a -> Snowflake b

type HasID' a = HasID a a

-- | A newtype wrapper for deriving HasID generically
newtype HasIDField field a = HasIDField a

instance (HasID b c, HasField' field a c) => HasID b (HasIDField field a) where
  getID (HasIDField a) = getID $ a ^. field' @field

-- | A data `a` which contains an ID of type `Snowflake c`
--   which should be swapped with `Snowflake b` upon fetching
newtype HasIDFieldCoerce field a c = HasIDFieldCoerce a

type HasIDFieldCoerce' field a = HasIDFieldCoerce field a a

instance (HasID c d, HasField' field a d) => HasID b (HasIDFieldCoerce field a c) where
  getID (HasIDFieldCoerce a) = coerceSnowflake . getID @c $ a ^. field' @field

instance HasID a (Snowflake a) where
  getID = id
