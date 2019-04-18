-- | Prelude for this library

module Prelude
  ( module Protolude
  -- , module Data.Generics.Sum
  -- , module Data.Generics.Product
  , module Control.Lens
  , module System.Log.Simple
  , module Fmt
  , module Data.Aeson.Lens
  , module Control.Arrow
  , LogMessage
  , debug
  , info
  , warning
  , error
  , fatal
  , trace
  , jsonOptions
  , jsonOptionsKeepNothing
  )
where

import           Data.Aeson              hiding ( Error )
import           Data.Aeson.Lens

import           Protolude               hiding ( HasField
                                                , getField
                                                , trace
                                                )
-- import           Data.Generics.Product   hiding ( list )
-- import           Data.Generics.Sum
import           Control.Lens            hiding ( Strict
                                                , Level
                                                , uncons
                                                , unsnoc
                                                , from
                                                , to
                                                , op
                                                , (<.>)
                                                , (.=)
                                                )
import           System.Log.Simple       hiding ( Message
                                                , Debug
                                                , Info
                                                , Warning
                                                , Error
                                                , Fatal
                                                , Trace
                                                , trace
                                                )
import qualified System.Log.Simple             as SLS
import           Fmt
import           Control.Arrow                  ( (>>>) )


type LogMessage = SLS.Message

debug, info, warning, error, fatal, trace :: MonadLog m => Text -> m ()
debug = sendLog SLS.Debug
info = sendLog SLS.Info
warning = sendLog SLS.Warning
error = sendLog SLS.Error
fatal = sendLog SLS.Fatal
trace = sendLog SLS.Trace



jsonOptions :: Options
jsonOptions = defaultOptions
  { sumEncoding        = UntaggedValue
  , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
  , omitNothingFields  = True
  }

jsonOptionsKeepNothing :: Options
jsonOptionsKeepNothing = defaultOptions
  { sumEncoding        = UntaggedValue
  , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
  , omitNothingFields  = False
  }
