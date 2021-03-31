-- | Discord colour utilities
module Calamity.Utils.Colour
    ( colourToWord64
    , colourFromWord64
    -- * Useful colours
    , blurple
    , greyple ) where

import           Calamity.Internal.IntColour

import           Data.Colour
import           Data.Colour.SRGB            ( sRGB24 )

blurple :: (Ord a, Floating a) => Colour a
blurple = sRGB24 0x72 0x89 0xda

greyple :: (Ord a, Floating a) => Colour a
greyple = sRGB24 0x99 0xaa 0xb5
