-- | Prelude for this library

module Prelude
  ( module Protolude
  -- , module Data.Generics.Sum
  -- , module Data.Generics.Product
  , module Control.Lens
  , module Control.Monad.Log
  , module Fmt
  , jsonOptions
  , jsonOptionsKeepNothing
  )
where

import           Data.Aeson

import           Protolude               hiding ( HasField
                                                , getField
                                                )
-- import           Data.Generics.Product   hiding ( list )
-- import           Data.Generics.Sum
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , (?~)
                                                , (?=)
                                                , (%=)
                                                , use
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
