-- | Message components
module Calamity.Types.Model.Channel.Component (
    Component (..),
    Button (..),
    button,
    button',
    ButtonStyle (..),
    ComponentType (..),
    componentType,
) where

import Calamity.Internal.AesonThings
import Calamity.Types.Model.Guild.Emoji
import Data.Aeson
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data Button = Button
    { style :: ButtonStyle
    , label :: Maybe T.Text
    , emoji :: Maybe RawEmoji
    , customID :: Maybe T.Text
    , url :: Maybe T.Text
    , disabled :: Bool
    }
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric Button
    deriving (ToJSON) via CalamityJSONKeepNothing Button
    deriving
        (FromJSON)
        via WithSpecialCases
                '["disabled" `IfNoneThen` DefaultToFalse]
                Button

data ButtonStyle
    = ButtonPrimary
    | ButtonSecondary
    | ButtonSuccess
    | ButtonDanger
    | ButtonLink
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric ButtonStyle

{- | Constuct a non-disabled 'Button' with the given 'ButtonStyle', all other
 fields are set to 'Nothing'
-}
button :: ButtonStyle -> Button
button s = Button s Nothing Nothing Nothing Nothing False

{- | Constuct a non-disabled 'Button' with the given 'ButtonStyle' and label,
 all other fields are set to 'Nothing'
-}
button' :: ButtonStyle -> T.Text -> Button
button' s l = Button s (Just l) Nothing Nothing Nothing False

instance ToJSON ButtonStyle where
    toJSON t = toJSON @Int $ case t of
        ButtonPrimary -> 1
        ButtonSecondary -> 2
        ButtonSuccess -> 3
        ButtonDanger -> 4
        ButtonLink -> 5

instance FromJSON ButtonStyle where
    parseJSON = withScientific "ChannelType" $ \n -> case toBoundedInteger @Int n of
        Just v -> case v of
            1 -> pure ButtonPrimary
            2 -> pure ButtonSecondary
            3 -> pure ButtonSuccess
            4 -> pure ButtonDanger
            5 -> pure ButtonLink
            _ -> fail $ "Invalid ButtonStyle: " <> show n
        Nothing -> fail $ "Invalid ButtonStyle: " <> show n

data Component
    = Button' Button
    | ActionRow' [Component]
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric Component

instance ToJSON Component where
    toJSON t =
        let (Object inner, type_) = case t of
                ActionRow' xs -> (Object ("components" .= xs), 1 :: Int)
                Button' b -> (toJSON b, 2 :: Int)
         in Object (inner <> ("type" .= type_))

instance FromJSON Component where
    parseJSON = withObject "Component" $ \v -> do
        type_ :: Int <- v .: "type"

        case type_ of
            1 -> do
                ActionRow' <$> v .: "components"
            2 -> Button' <$> parseJSON (Object v)
            _ -> fail $ "Invalid ComponentType: " <> show type_

componentType :: Component -> ComponentType
componentType (ActionRow' _) = ActionRowType
componentType (Button' _) = ButtonType

data ComponentType
    = ActionRowType
    | ButtonType
    deriving (Show, Generic)
    deriving (TextShow) via TSG.FromGeneric ComponentType

instance ToJSON ComponentType where
    toJSON x = toJSON @Int $ case x of
        ActionRowType -> 1
        ButtonType -> 2

instance FromJSON ComponentType where
    parseJSON = withScientific "ComponentType" $ \n -> case toBoundedInteger @Int n of
        Just 1 -> pure ActionRowType
        Just 2 -> pure ButtonType
        _ -> fail $ "Invalid ComponentType: " <> show n
