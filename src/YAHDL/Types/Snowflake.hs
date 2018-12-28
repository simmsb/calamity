-- | The snowflake type

module YAHDL.Types.Snowflake where

import           Text.Read                      ( read )
import           Data.Text                      ( unpack )
import           Data.Aeson

-- Thanks sbrg
-- https://github.com/saevarb/haskord/blob/d1bb07bcc4f3dbc29f2dfd3351ff9f16fc100c07/haskord-lib/src/Haskord/Types/Common.hs#L78
newtype Snowflake t = Snowflake Word64
  deriving (Generic, Show, Eq)

instance Hashable (Snowflake t)

instance ToJSON (Snowflake t) where
  toJSON (Snowflake s) = String . show $ s

instance FromJSON (Snowflake t) where
  parseJSON = withText "Snowflake" $ pure . Snowflake . read . unpack

fromSnowflake :: Snowflake t -> Word64
fromSnowflake (Snowflake s) = s
