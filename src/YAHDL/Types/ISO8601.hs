-- | Parsing of iso8601 timestamps

module YAHDL.Types.ISO8601
  ( ISO8601Timestamp(..)
  )
where

import           Control.Monad
import           Data.Aeson
import qualified Data.Text.Encoding            as E
import           Data.Time
import           Network.HTTP.Date


newtype ISO8601Timestamp = ISO8601Timestamp UTCTime
  deriving (Show, Eq, Generic)

instance FromJSON ISO8601Timestamp where
  parseJSON = withText "ISO8601Timestamp" $ \v ->
    case parseHTTPDate . E.encodeUtf8 $ v of
      Just dt -> pure . ISO8601Timestamp . httpDateToUTC $ dt
      Nothing -> fail $ "Could not decode datetime: "+|v|+""

instance ToJSON ISO8601Timestamp where
  toEncoding (ISO8601Timestamp dt) = toEncoding . E.decodeUtf8 . formatHTTPDate . utcToHTTPDate $ dt
