-- | Parsing of unix timestamps

module Calamity.Types.UnixTimestamp
  ( UnixTimestamp(..)
  )
where

import           Control.Arrow                  ( (>>>) )
import           Data.Aeson
import           Data.Aeson.Encoding            ( word64 )
import           Data.Time
import           Data.Time.Clock.POSIX


newtype UnixTimestamp = UnixTimestamp { unUnixTimestamp :: UTCTime }
  deriving (Show, Eq, Generic)

instance ToJSON UnixTimestamp where
  toEncoding = unUnixTimestamp
               >>> utcTimeToPOSIXSeconds
               >>> toRational
               >>> (* 1000)
               >>> round
               >>> word64

instance FromJSON UnixTimestamp where
  parseJSON = withScientific "UnixTimestamp" $
    toRational
    >>> (/ 1000)
    >>> fromRational
    >>> posixSecondsToUTCTime
    >>> UnixTimestamp
    >>> pure
