module Calamity.Internal.AesonThings
    ( WithSpecialCases(..)
    , IfNoneThen
    , ExtractField
    , ExtractFields
    , ExtractArrayField
    , DefaultToEmptyArray
    , DefaultToZero
    , DefaultToFalse
    , CalamityJSON(..)
    , jsonOptions
    , jsonOptionsKeepNothing ) where

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types      ( Parser )
import           Data.Kind
import           Data.Reflection       ( Reifies(..) )
import           Data.Text             ( Text )
import           Data.Text.Strict.Lens
import           Data.Typeable

import           GHC.Generics
import           GHC.TypeLits          ( KnownSymbol, symbolVal )
import Control.Monad ((>=>))

textSymbolVal :: forall n. KnownSymbol n => Text
textSymbolVal = symbolVal @n Proxy ^. packed

data IfNoneThen label def
data ExtractField label field
data ExtractFields label fields
data ExtractArrayField label field

class PerformAction action where
  runAction :: Proxy action -> Object -> Parser Object

instance (Reifies d Value, KnownSymbol label) => PerformAction (IfNoneThen label d) where
  runAction _ o = do
    v <- o .:? textSymbolVal @label .!= reflect @d Proxy
    pure $ o & at (textSymbolVal @label) ?~ v

instance (KnownSymbol label, KnownSymbol field) => PerformAction (ExtractField label field) where
  runAction _ o =
    let v :: Maybe Value = o ^? ix (textSymbolVal @label) . _Object . ix (textSymbolVal @field)
    in pure $ o & at (textSymbolVal @field) .~ v

instance PerformAction (ExtractFields label '[]) where
  runAction _ = pure

instance (KnownSymbol field,
          PerformAction (ExtractField label field),
          PerformAction (ExtractFields label fields)) =>
         PerformAction (ExtractFields label (field : fields)) where
  runAction _ = runAction (Proxy @(ExtractField label field)) >=> runAction (Proxy @(ExtractFields label fields))

instance (KnownSymbol label, KnownSymbol field) => PerformAction (ExtractArrayField label field) where
  runAction _ o = do
    a :: Array <- o .: textSymbolVal @label
    a' <- Array <$> traverse (withObject "extracting field" (.: textSymbolVal @field)) a
    pure $ o & at (textSymbolVal @label) ?~ a'

newtype WithSpecialCases (rules :: [Type]) a = WithSpecialCases a

class RunSpecialCase a where
  runSpecialCases :: Proxy a -> Object -> Parser Object

instance RunSpecialCase '[] where
  runSpecialCases _ = pure . id

instance (RunSpecialCase xs, PerformAction action) => RunSpecialCase (action : xs) where
  runSpecialCases _ o = do
    o' <- runSpecialCases (Proxy @xs) o
    runAction (Proxy @action) o'

instance (RunSpecialCase rules, Typeable a, Generic a, GFromJSON Zero (Rep a))
  => FromJSON (WithSpecialCases rules a) where
  parseJSON = withObject (show . typeRep $ Proxy @a) $ \o -> do
    o' <- runSpecialCases (Proxy @rules) o
    WithSpecialCases <$> genericParseJSON jsonOptions (Object o')


data DefaultToEmptyArray

instance Reifies DefaultToEmptyArray Value where
  reflect _ = Array mempty

data DefaultToZero

instance Reifies DefaultToZero Value where
  reflect _ = Number 0

data DefaultToFalse

instance Reifies DefaultToFalse Value where
  reflect _ = Bool False

newtype CalamityJSON a = CalamityJSON
  { unCalamityJSON :: a
  }

instance (Typeable a, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (CalamityJSON a) where
  toJSON = genericToJSON jsonOptions . unCalamityJSON

  toEncoding = genericToEncoding jsonOptions . unCalamityJSON

instance (Typeable a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (CalamityJSON a) where
  parseJSON = fmap CalamityJSON . genericParseJSON jsonOptions

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding        = UntaggedValue
                             , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
                             , omitNothingFields  = True }

jsonOptionsKeepNothing :: Options
jsonOptionsKeepNothing = defaultOptions { sumEncoding        = UntaggedValue
                                        , fieldLabelModifier = camelTo2 '_' . filter (/= '_')
                                        , omitNothingFields  = False }
