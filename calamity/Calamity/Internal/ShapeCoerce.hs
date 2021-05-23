{- | Something to coerce between two records with the same field names and
 compatible field types
-}
module Calamity.Internal.ShapeCoerce (
    shapeCoerce,
    ShapeCoerce,
    GShapeCoerce (..),
) where

import Data.Coerce (Coercible, coerce)
import GHC.Generics

type ShapeCoerce a b = (Generic a, Generic b, GShapeCoerce (Rep a) (Rep b))

shapeCoerce :: (Generic a, Generic b, GShapeCoerce (Rep a) (Rep b)) => a -> b
shapeCoerce = to . gshapeCoerce . from

class GShapeCoerce f g where
    gshapeCoerce :: f a -> g b

instance (GShapeCoerce fa ga, GShapeCoerce fb gb) => GShapeCoerce (fa :*: fb) (ga :*: gb) where
    gshapeCoerce (la :*: lb) = gshapeCoerce la :*: gshapeCoerce lb

instance GShapeCoerce f g => GShapeCoerce (M1 D da f) (M1 D db g) where
    gshapeCoerce (M1 a) = M1 (gshapeCoerce a)

instance GShapeCoerce f g => GShapeCoerce (M1 C da f) (M1 C db g) where
    gshapeCoerce (M1 a) = M1 (gshapeCoerce a)

instance GShapeCoerce f g => GShapeCoerce (S1 ( 'MetaSel ( 'Just name) fsu fss fl) f) (S1 ( 'MetaSel ( 'Just name') gsu gss gl) g) where
    gshapeCoerce (M1 a) = M1 (gshapeCoerce a)

instance GShapeCoerce f g => GShapeCoerce (S1 ( 'MetaSel 'Nothing fsu fss fl) f) (S1 ( 'MetaSel 'Nothing gsu gss gl) g) where
    gshapeCoerce (M1 a) = M1 (gshapeCoerce a)

instance Coercible a b => GShapeCoerce (Rec0 a) (Rec0 b) where
    gshapeCoerce (K1 a) = K1 $ coerce a
