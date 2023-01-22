-- | Internal newtype for deserializing timestamps
module Calamity.Internal.UnixTimestamp (
  UnixTimestamp (..),
  unixToMilliseconds,
  millisecondsToUnix,
) where

import Calamity.Internal.Utils ()
import Control.Arrow
import Data.Aeson
import Data.Aeson.Encoding (word64)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import TextShow

newtype UnixTimestamp = UnixTimestamp
  { unUnixTimestamp :: UTCTime
  }
  deriving (Show) via UTCTime
  deriving (TextShow) via FromStringShow UTCTime

unixToMilliseconds :: UnixTimestamp -> Word64
unixToMilliseconds =
  unUnixTimestamp
    >>> utcTimeToPOSIXSeconds
    >>> toRational
    >>> (* 1000)
    >>> round

millisecondsToUnix :: Word64 -> UnixTimestamp
millisecondsToUnix =
  toRational
    >>> fromRational
    >>> (/ 1000)
    >>> posixSecondsToUTCTime
    >>> UnixTimestamp

instance ToJSON UnixTimestamp where
  toJSON =
    unUnixTimestamp
      >>> utcTimeToPOSIXSeconds
      >>> toRational
      >>> round
      >>> toJSON @Word64
  toEncoding =
    unUnixTimestamp
      >>> utcTimeToPOSIXSeconds
      >>> toRational
      >>> round
      >>> word64

instance FromJSON UnixTimestamp where
  parseJSON =
    withScientific "UnixTimestamp" $
      toRational
        >>> fromRational
        >>> posixSecondsToUTCTime
        >>> UnixTimestamp
        >>> pure
