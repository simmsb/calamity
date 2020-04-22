-- | Parsing of unix timestamps
module Calamity.Types.UnixTimestamp
    ( UnixTimestamp(..)
    , unixToMilliseconds
    , millisecondsToUnix ) where

import           Calamity.Internal.Utils ()

import           Control.Arrow

import           Data.Aeson
import           Data.Aeson.Encoding     ( word64 )
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic        as TSG

newtype UnixTimestamp = UnixTimestamp
  { unUnixTimestamp :: UTCTime
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric UnixTimestamp

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
