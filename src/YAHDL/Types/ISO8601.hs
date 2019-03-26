-- | Parsing of iso8601 timestamps

module YAHDL.Types.ISO8601
  ( ISO8601Timestamp(..)
  )
where

import           Data.Aeson
import           Data.Time


newtype ISO8601Timestamp = ISO8601Timestamp { unISO8601Timestamp :: ZonedTime }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Eq ISO8601Timestamp where
  ISO8601Timestamp a == ISO8601Timestamp b =
    zonedTimeToLocalTime a == zonedTimeToLocalTime b &&
    zonedTimeZone a == zonedTimeZone b
