{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An orphan instnace to parse @'Data.Colour' 'Double'@ as a word64
module Calamity.Internal.IntColour
    ( colourToWord64
    , colourFromWord64 ) where

import           Data.Aeson
import           Data.Bits
import           Data.Colour
import           Data.Colour.SRGB ( RGB(RGB), sRGB24, toSRGB24 )
import           Data.Word        ( Word64 )

colourToWord64 :: Colour Double -> Word64
colourToWord64 c = let RGB r g b = toSRGB24 c
                       i         = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b
                   in i

colourFromWord64 :: Word64 -> Colour Double
colourFromWord64 i = let r = (i `shiftR` 16) .&. 0xff
                         g = (i `shiftR` 8) .&. 0xff
                         b = i .&. 0xff
                     in sRGB24 (fromIntegral r) (fromIntegral g) (fromIntegral b)

instance ToJSON (Colour Double) where
  toJSON = toJSON . colourToWord64

instance FromJSON (Colour Double) where
  parseJSON v = colourFromWord64 <$> parseJSON v
