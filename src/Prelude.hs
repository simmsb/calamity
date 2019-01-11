-- | Prelude for this library

module Prelude
  ( module Protolude
  -- , module Data.Generics.Sum
  -- , module Data.Generics.Product
  , module Control.Lens
  , module Control.Monad.Log
  , module Fmt
  , module Data.Aeson.Lens
  , jsonOptions
  , jsonOptionsKeepNothing
  )
where

import           Data.Aeson
import           Data.Aeson.Lens

import           Protolude               hiding ( HasField
                                                , getField
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
import           Control.Monad.Log              ( debug
                                                , info
                                                , warning
                                                , error
                                                )
import           Fmt


jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding        = UntaggedValue
                             , fieldLabelModifier = camelTo2 '_'
                             , omitNothingFields  = True
                             }

jsonOptionsKeepNothing :: Options
jsonOptionsKeepNothing = defaultOptions { sumEncoding        = UntaggedValue
                                        , fieldLabelModifier = camelTo2 '_'
                                        , omitNothingFields  = False
                                        }
