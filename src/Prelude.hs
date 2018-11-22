-- | Prelude for this library

module Prelude
  ( module Protolude
  -- , module Data.Generics.Sum
  -- , module Data.Generics.Product
  , module Control.Lens
  )
where

import           Protolude               hiding ( HasField
                                                , getField
                                                , (.=)
                                                )
-- import           Data.Generics.Product   hiding ( list )
-- import           Data.Generics.Sum
import           Control.Lens                   ( (^.)
                                                , (.=)
                                                , (?=)
                                                )
