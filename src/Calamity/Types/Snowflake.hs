-- | The snowflake type

module Calamity.Types.Snowflake
  ( Snowflake(..)
  , HasID(..)
  , coerceSnowflake
  )
where

import           Data.Aeson
import           Data.Data
import           Data.Generics.Product.Fields
import           Data.Text                      ( unpack )
import           Text.Read                      ( read )

-- Thanks sbrg
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs#L78
newtype Snowflake t = Snowflake { fromSnowflake :: Word64 }
  deriving (Generic, Show, Eq, Ord, Data, NFData, ToJSONKey)

instance Hashable (Snowflake t)

instance ToJSON (Snowflake t) where
  toJSON (Snowflake s) = String . show $ s

instance FromJSON (Snowflake t) where
  parseJSON = withText "Snowflake" $ pure . Snowflake . read . unpack

coerceSnowflake :: Snowflake a -> Snowflake b
coerceSnowflake (Snowflake t) = Snowflake t

class HasID a where
  getID :: a -> Snowflake a

instance {-# OVERLAPPABLE #-} HasField "id" a a (Snowflake a) (Snowflake a) => HasID a where
  getID a = a ^. field @"id"
