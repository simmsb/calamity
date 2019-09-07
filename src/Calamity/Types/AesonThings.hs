module Calamity.Types.AesonThings
    ( WithSpecialCases
    , type IfNoneThen
    , SpecialRule
    , EmptyList ) where

import           Data.Aeson
import           Data.Reflection       ( Reifies(..) )
import           Data.Text.Strict.Lens
import           Data.Typeable         ( typeRep )

import           GHC.Exts              ( fromList )
import           GHC.TypeLits          ( KnownSymbol, symbolVal )

import qualified Debug.Trace as DT

data SpecialCaseList
  = SpecialCaseNil
  | forall inner action label. SpecialCaseElem label action inner

data SpecialRule (label :: Symbol) (action :: SpecialRuleAction)

data SpecialRuleAction = forall d. IfNoneThen d

type IfNoneThen = 'IfNoneThen

class PerformAction (action :: SpecialRuleAction) where
  runAction :: Proxy action -> Maybe Value -> Value

instance Reifies d Value => PerformAction ('IfNoneThen d) where
  runAction _ (Just Null) = reflect @d Proxy
  runAction _ Nothing     = reflect @d Proxy
  runAction _ (Just x)    = x

type family FoldSpecialCases (rules :: [Type]) :: SpecialCaseList where
  FoldSpecialCases '[]                              = 'SpecialCaseNil
  FoldSpecialCases (SpecialRule label action ': xs) = 'SpecialCaseElem label action (FoldSpecialCases xs)

newtype WithSpecialCasesInner (rules :: SpecialCaseList) a = WithSpecialCasesInner a

type family WithSpecialCases rules a :: Type where
  WithSpecialCases rules a = WithSpecialCasesInner (FoldSpecialCases rules) a

class RunSpecialCase a where
  runSpecialCases :: Proxy a -> Object -> Object

instance RunSpecialCase 'SpecialCaseNil where
  runSpecialCases _ = identity

instance (RunSpecialCase inner, KnownSymbol label, PerformAction action)
  => RunSpecialCase ('SpecialCaseElem label action inner) where
  runSpecialCases _ o = let o' = runSpecialCases (Proxy @inner) o
                            v  = o' ^. at (symbolVal @label Proxy ^. packed)
                            v' = runAction (Proxy @action) v
                        in o'
                           & at (symbolVal @label Proxy ^. packed) ?~ v'

instance (RunSpecialCase rules, Typeable a, Generic a, GFromJSON Zero (Rep a))
  => FromJSON (WithSpecialCasesInner rules a) where
  parseJSON = withObject (show . typeRep $ Proxy @a) $ \o
    -> let o' = runSpecialCases (Proxy @rules) o
       in WithSpecialCasesInner <$> genericParseJSON jsonOptions (Object o')

data EmptyList

instance Reifies EmptyList Value where
  reflect _ = Array (fromList [])
