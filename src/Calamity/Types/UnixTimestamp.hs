-- | Parsing of unix timestamps
module Calamity.Types.UnixTimestamp
    ( UnixTimestamp(..)
    , unixToMilliseconds
    , millisecondsToUnix ) where

import           Data.Aeson
import           Data.Aeson.Encoding   ( word64 )
import           Data.Time
import           Data.Time.Clock.POSIX

newtype UnixTimestamp = UnixTimestamp
  { unUnixTimestamp :: UTCTime
  }
  deriving ( Show, Eq, Generic )

unixToMilliseconds :: UnixTimestamp -> Word64
unixToMilliseconds = unUnixTimestamp
                     >>> utcTimeToPOSIXSeconds
                     >>> toRational
                     >>> (* 1000)
                     >>> round

millisecondsToUnix :: Word64 -> UnixTimestamp
millisecondsToUnix = toRational
                     >>> fromRational
                     >>> (/ 1000)
                     >>> posixSecondsToUTCTime
                     >>> UnixTimestamp

instance ToJSON UnixTimestamp where
  toEncoding = unUnixTimestamp
               >>> utcTimeToPOSIXSeconds
               >>> toRational
               >>> round
               >>> word64

instance FromJSON UnixTimestamp where
  parseJSON = withScientific "UnixTimestamp" $
    toRational
    >>> fromRational
    >>> posixSecondsToUTCTime
    >>> UnixTimestamp
    >>> pure
