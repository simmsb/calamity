-- | Internal newtype that we can safely define orphans on
module Calamity.Internal.OverriddenVia (
    OverriddenVia (..),
) where

import Calamity.Internal.ShapeCoerce
import Data.Aeson.Types
import Data.Default.Class
import TextShow

{- | @a@: The type that is to be wrapped
 @b@: The type to convert to and use the instance of
-}
newtype OverriddenVia a b = OverriddenVia {unOverrideVia :: a}

instance (ShapeCoerce b a, Default b) => Default (OverriddenVia a b) where
    def = OverriddenVia $ shapeCoerce @b @a $ def

instance (ShapeCoerce a b, Show b) => Show (OverriddenVia a b) where
    showsPrec d (OverriddenVia x) = showsPrec d $ shapeCoerce @a @b x
    show (OverriddenVia x) = show $ shapeCoerce @a @b x
    showList xs = showList $ map (shapeCoerce @a @b . unOverrideVia) xs

instance (ShapeCoerce b a, FromJSON b) => FromJSON (OverriddenVia a b) where
    parseJSON v = OverriddenVia . shapeCoerce @b @a <$> parseJSON v
    parseJSONList v = map (OverriddenVia . shapeCoerce @b @a) <$> parseJSONList v

instance (ShapeCoerce a b, ToJSON b) => ToJSON (OverriddenVia a b) where
    toJSON = toJSON . shapeCoerce @a @b . unOverrideVia
    toEncoding = toEncoding . shapeCoerce @a @b . unOverrideVia

    toJSONList = toJSONList . map (shapeCoerce @a @b . unOverrideVia)
    toEncodingList = toEncodingList . map (shapeCoerce @a @b . unOverrideVia)

instance (ShapeCoerce a b, TextShow b) => TextShow (OverriddenVia a b) where
    showb = showb . shapeCoerce @a @b . unOverrideVia
