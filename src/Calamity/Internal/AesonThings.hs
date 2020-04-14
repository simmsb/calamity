{-# LANGUAGE NoImplicitPrelude #-}

module Calamity.Internal.AesonThings
    ( WithSpecialCases
    , WithSpecialCasesInner(..)
    , type IfNoneThen
    , type ExtractField
    , type InjectID
    , SpecialRule
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
import           Data.Default.Class
import           Data.Reflection       ( Reifies(..) )
import           Data.Text.Strict.Lens
import           Data.Typeable         ( typeRep )

import qualified GHC.TypeLits          as TL
import           GHC.TypeLits          ( KnownSymbol, symbolVal )

import           Protolude

textSymbolVal :: forall n. KnownSymbol n => Proxy n -> Text
textSymbolVal _ = symbolVal @n Proxy ^. packed

data SpecialCaseList
  = SpecialCaseNil
  | forall label action inner. SpecialCaseElem label action inner

data SpecialRule (label :: Symbol) (action :: SpecialRuleAction)

data SpecialRuleAction
  = forall d. IfNoneThen d
  | forall field. ExtractField field
  | forall mn idn. InjectID idn mn

type IfNoneThen label d =
  SpecialRule label ('IfNoneThen d)

type ExtractField label field =
  SpecialRule label ('ExtractField field)

type InjectID label mn idn =
  SpecialRule label ('InjectID mn idn)

class PerformAction (action :: SpecialRuleAction) where
  runAction :: Proxy action -> Value -> Parser Value

instance Reifies d Value => PerformAction ('IfNoneThen d) where
  runAction _ Null = pure $ reflect @d Proxy
  runAction _ x = pure x

instance (KnownSymbol field) => PerformAction ('ExtractField field) where
  runAction _ Null = pure Null
  runAction _ o = (withObject (("extracting field " <> textSymbolVal @field Proxy) ^. unpacked) $ \o -> o
                   .: (textSymbolVal @field Proxy)) o

instance (KnownSymbol idn, KnownSymbol mn) => PerformAction ('InjectID idn mn) where
  runAction _ = withObject
    (("injecting id from " <> textSymbolVal @idn Proxy <> " into " <> textSymbolVal @mn Proxy) ^. unpacked) $ \o -> do
      id <- o .: "id"

      pure (Object o
            & key (textSymbolVal @mn Proxy) . values . _Object . at (textSymbolVal @idn Proxy) ?~ id)

type family FoldSpecialCases (rules :: [Type]) :: SpecialCaseList where
  FoldSpecialCases '[]                              = 'SpecialCaseNil
  FoldSpecialCases (SpecialRule label action ': xs) = 'SpecialCaseElem label action (FoldSpecialCases xs)
  FoldSpecialCases _ = TL.TypeError ('TL.Text "What did you do?")

newtype WithSpecialCasesInner (rules :: SpecialCaseList) a = WithSpecialCasesInner a

type family WithSpecialCases rules a :: Type where
  WithSpecialCases rules a = WithSpecialCasesInner (FoldSpecialCases rules) a

class RunSpecialCase a where
  runSpecialCases :: Proxy a -> Object -> Parser Object

instance RunSpecialCase 'SpecialCaseNil where
  runSpecialCases _ = pure . identity

instance (RunSpecialCase inner, KnownSymbol label, PerformAction action)
  => RunSpecialCase ('SpecialCaseElem label action inner) where
  runSpecialCases _ o = do
    o' <- runSpecialCases (Proxy @inner) o
    v <- o' .:? textSymbolVal @label Proxy .!= Null
    v' <- runAction (Proxy @action) v
    pure (o' & at (textSymbolVal @label Proxy) ?~ v')

instance (RunSpecialCase rules, Typeable a, Generic a, GFromJSON Zero (Rep a))
  => FromJSON (WithSpecialCasesInner rules a) where
  parseJSON = withObject (show . typeRep $ Proxy @a) $ \o -> do
    o' <- runSpecialCases (Proxy @rules) o
    WithSpecialCasesInner <$> genericParseJSON jsonOptions (Object o')


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
