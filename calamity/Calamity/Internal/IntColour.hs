{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket due to operator fixities" #-}

-- | An internal newtype for parsing colours
module Calamity.Internal.IntColour (
  IntColour (..),
  colourToWord64,
  colourFromWord64,
) where

import Data.Aeson
import Data.Bits
import Data.Colour
import Data.Colour.SRGB (RGB (RGB), sRGB24, toSRGB24)
import Data.Word (Word64)
import TextShow

newtype IntColour = IntColour
  { fromIntColour :: Colour Double
  }
  deriving (Show) via Colour Double
  deriving (TextShow) via FromStringShow (Colour Double)

colourToWord64 :: IntColour -> Word64
colourToWord64 (IntColour c) =
  let RGB r g b = toSRGB24 c
      i = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b
   in i

colourFromWord64 :: Word64 -> IntColour
colourFromWord64 i =
  let r = (i `shiftR` 16) .&. 0xff
      g = (i `shiftR` 8) .&. 0xff
      b = i .&. 0xff
   in IntColour $ sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b)

instance ToJSON IntColour where
  toJSON = toJSON . colourToWord64

instance FromJSON IntColour where
  parseJSON v = colourFromWord64 <$> parseJSON v
