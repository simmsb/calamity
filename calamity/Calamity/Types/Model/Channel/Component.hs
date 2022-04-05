-- | Message components
module Calamity.Types.Model.Channel.Component (
  CustomID (..),
  Component (..),
  Button (..),
  LinkButton (..),
  button,
  button',
  lbutton,
  lbutton',
  ButtonStyle (..),
  Select (..),
  select,
  TextInput (..),
  TextInputStyle (..),
  textInput,
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

newtype CustomID = CustomID T.Text
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric CustomID
  deriving (ToJSON, FromJSON) via T.Text

data Button = Button
  { style :: ButtonStyle
  , label :: Maybe T.Text
  , emoji :: Maybe RawEmoji
  , disabled :: Bool
  , customID :: CustomID
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric Button
  deriving (ToJSON) via CalamityJSONKeepNothing Button
  deriving
    (FromJSON)
    via WithSpecialCases
          '["disabled" `IfNoneThen` DefaultToFalse]
          Button

data LinkButton = LinkButton
  { stype :: ButtonStyle
  , label :: Maybe T.Text
  , emoji :: Maybe RawEmoji
  , url :: T.Text
  , disabled :: Bool
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric LinkButton
  deriving (ToJSON) via CalamityJSONKeepNothing LinkButton
  deriving
    (FromJSON)
    via WithSpecialCases
          '["disabled" `IfNoneThen` DefaultToFalse]
          LinkButton

data ButtonStyle
  = ButtonPrimary
  | ButtonSecondary
  | ButtonSuccess
  | ButtonDanger
  | ButtonLink
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric ButtonStyle

instance ToJSON ButtonStyle where
  toJSON t = toJSON @Int $ case t of
    ButtonPrimary -> 1
    ButtonSecondary -> 2
    ButtonSuccess -> 3
    ButtonDanger -> 4
    ButtonLink -> 5

instance FromJSON ButtonStyle where
  parseJSON = withScientific "ButtonStyle" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      1 -> pure ButtonPrimary
      2 -> pure ButtonSecondary
      3 -> pure ButtonSuccess
      4 -> pure ButtonDanger
      5 -> pure ButtonLink
      _ -> fail $ "Invalid ButtonStyle: " <> show n
    Nothing -> fail $ "Invalid ButtonStyle: " <> show n

{- | Constuct a non-disabled 'Button' with the given 'ButtonStyle' and 'CustomID',
 all other fields are set to 'Nothing'
-}
button :: ButtonStyle -> CustomID -> Button
button s = Button s Nothing Nothing False

{- | Constuct a non-disabled 'Button' with the given 'ButtonStyle', 'CustomID',
 and label, all other fields are set to 'Nothing'
-}
button' :: ButtonStyle -> T.Text -> CustomID -> Button
button' s l = Button s (Just l) Nothing False

{- | Constuct a non-disabled 'LinkButton' with the given 'ButtonStyle', link, all
   other fields are set to 'Nothing'
-}
lbutton ::
  ButtonStyle ->
  -- | The link to use
  T.Text ->
  LinkButton
lbutton s lnk = LinkButton s Nothing Nothing lnk False

{- | Constuct a non-disabled 'LinkButton' with the given 'ButtonStyle', link,
 and label, all other fields are set to 'Nothing'
-}
lbutton' ::
  ButtonStyle ->
  -- | The link to use
  T.Text ->
  -- | The label to use
  T.Text ->
  LinkButton
lbutton' s lnk lbl = LinkButton s (Just lbl) Nothing lnk False

data Select = Select
  { options :: [SelectOption]
  , placeholder :: Maybe T.Text
  , minValues :: Maybe Int
  , maxValues :: Maybe Int
  , disabled :: Bool
  , customID :: CustomID
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric Select
  deriving (ToJSON) via CalamityJSONKeepNothing Select
  deriving
    (FromJSON)
    via WithSpecialCases
          '["disabled" `IfNoneThen` DefaultToFalse]
          Select

data SelectOption = SelectOption
  { label :: T.Text
  , value :: T.Text
  , description :: Maybe T.Text
  , emoji :: Maybe RawEmoji
  , default_ :: Bool
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric SelectOption
  deriving (ToJSON) via CalamityJSONKeepNothing SelectOption
  deriving
    (FromJSON)
    via WithSpecialCases
          '["disabled" `IfNoneThen` DefaultToFalse]
          SelectOption

select :: [SelectOption] -> CustomID -> Select
select o = Select o Nothing Nothing Nothing False

data TextInput = TextInput
  { style :: TextInputStyle
  , label :: T.Text
  , minLength :: Maybe Int
  , maxLength :: Maybe Int
  , required :: Bool
  , value :: Maybe T.Text
  , placeholder :: Maybe T.Text
  , customID :: CustomID
  }
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric TextInput
  deriving (ToJSON) via CalamityJSONKeepNothing TextInput
  deriving
    (FromJSON)
    via WithSpecialCases
          '["required" `IfNoneThen` DefaultToFalse]
          TextInput

data TextInputStyle
  = TextInputShort
  | TextInputParagraph
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric TextInputStyle

instance ToJSON TextInputStyle where
  toJSON t = toJSON @Int $ case t of
    TextInputShort -> 1
    TextInputParagraph -> 2

instance FromJSON TextInputStyle where
  parseJSON = withScientific "TextInputStyle" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      1 -> pure TextInputShort
      2 -> pure TextInputParagraph
      _ -> fail $ "Invalid TextInputStyle: " <> show n
    Nothing -> fail $ "Invalid TextInputStyle: " <> show n

textInput :: TextInputStyle -> T.Text -> CustomID -> TextInput
textInput s l = TextInput s l Nothing Nothing True Nothing Nothing

data Component
  = ActionRow' [Component]
  | Button' Button
  | LinkButton' LinkButton
  | Select' Select
  | TextInput' TextInput
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric Component

instance ToJSON Component where
  toJSON t =
    let (Object inner, type_) = case t of
          ActionRow' xs -> (Object ("components" .= xs), 1 :: Int)
          Button' b -> (toJSON b, 2 :: Int)
          LinkButton' lb -> (toJSON lb, 2 :: Int)
          Select' s -> (toJSON s, 3 :: Int)
          TextInput' ti -> (toJSON ti, 4 :: Int)
     in Object (inner <> ("type" .= type_))

instance FromJSON Component where
  parseJSON = withObject "Component" $ \v -> do
    type_ :: Int <- v .: "type"

    case type_ of
      1 -> ActionRow' <$> v .: "components"
      2 -> do
        cid :: Maybe CustomID <- v .:? "custom_id"
        url :: Maybe T.Text <- v .:? "url"
        case (cid, url) of
          (Just _, _) -> Button' <$> parseJSON (Object v)
          (_, Just _) -> LinkButton' <$> parseJSON (Object v)
          _ -> fail $ "Impossible button: " <> show v
      3 -> Select' <$> parseJSON (Object v)
      4 -> TextInput' <$> parseJSON (Object v)
      _ -> fail $ "Invalid ComponentType: " <> show type_

componentType :: Component -> ComponentType
componentType (ActionRow' _) = ActionRowType
componentType (Button' _) = ButtonType
componentType (LinkButton' _) = ButtonType
componentType (Select' _) = SelectType
componentType (TextInput' _) = TextInputType

data ComponentType
  = ActionRowType
  | ButtonType
  | SelectType
  | TextInputType
  deriving (Show, Generic)
  deriving (TextShow) via TSG.FromGeneric ComponentType

instance ToJSON ComponentType where
  toJSON x = toJSON @Int $ case x of
    ActionRowType -> 1
    ButtonType -> 2
    SelectType -> 3
    TextInputType -> 4

instance FromJSON ComponentType where
  parseJSON = withScientific "ComponentType" $ \n -> case toBoundedInteger @Int n of
    Just 1 -> pure ActionRowType
    Just 2 -> pure ButtonType
    Just 3 -> pure SelectType
    Just 4 -> pure TextInputType
    _ -> fail $ "Invalid ComponentType: " <> show n
