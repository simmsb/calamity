{-# LANGUAGE TemplateHaskell #-}

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
  SelectOption (..),
  sopt,
  TextInput (..),
  TextInputStyle (..),
  textInput,
  ComponentType (..),
  componentType,
) where

import Calamity.Internal.Utils (CalamityToJSON (..), CalamityToJSON' (..), (.=), (.?=))
import Calamity.Types.Model.Guild.Emoji
import Control.Monad (replicateM)
import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import Optics.TH
import System.Random (Uniform)
import System.Random.Stateful (Uniform (uniformM), UniformRange (uniformRM))
import TextShow.TH
import Data.Maybe (catMaybes)

newtype CustomID = CustomID T.Text
  deriving stock (Eq, Ord, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via T.Text

$(deriveTextShow ''CustomID)

instance Uniform CustomID where
  uniformM = ((CustomID . T.pack) <$>) . replicateM 16 . uniformRM ('a', 'z')

data ComponentType
  = ActionRowType
  | ButtonType
  | SelectType
  | TextInputType
  deriving (Eq, Show)

$(deriveTextShow ''ComponentType)

instance Aeson.ToJSON ComponentType where
  toJSON x = Aeson.toJSON @Int $ case x of
    ActionRowType -> 1
    ButtonType -> 2
    SelectType -> 3
    TextInputType -> 4
  toEncoding x = Aeson.toEncoding @Int $ case x of
    ActionRowType -> 1
    ButtonType -> 2
    SelectType -> 3
    TextInputType -> 4

instance Aeson.FromJSON ComponentType where
  parseJSON = Aeson.withScientific "Components.ComponentType" $ \n -> case toBoundedInteger @Int n of
    Just 1 -> pure ActionRowType
    Just 2 -> pure ButtonType
    Just 3 -> pure SelectType
    Just 4 -> pure TextInputType
    _ -> fail $ "Invalid ComponentType: " <> show n

data ButtonStyle
  = ButtonPrimary
  | ButtonSecondary
  | ButtonSuccess
  | ButtonDanger
  | ButtonLink
  deriving (Eq, Show)

$(deriveTextShow ''ButtonStyle)

instance Aeson.ToJSON ButtonStyle where
  toJSON t = Aeson.toJSON @Int $ case t of
    ButtonPrimary -> 1
    ButtonSecondary -> 2
    ButtonSuccess -> 3
    ButtonDanger -> 4
    ButtonLink -> 5
  toEncoding t = Aeson.toEncoding @Int $ case t of
    ButtonPrimary -> 1
    ButtonSecondary -> 2
    ButtonSuccess -> 3
    ButtonDanger -> 4
    ButtonLink -> 5

instance Aeson.FromJSON ButtonStyle where
  parseJSON = Aeson.withScientific "Components.ButtonStyle" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      1 -> pure ButtonPrimary
      2 -> pure ButtonSecondary
      3 -> pure ButtonSuccess
      4 -> pure ButtonDanger
      5 -> pure ButtonLink
      _ -> fail $ "Invalid ButtonStyle: " <> show n
    Nothing -> fail $ "Invalid ButtonStyle: " <> show n

data Button = Button
  { style :: ButtonStyle
  , label :: Maybe T.Text
  , emoji :: Maybe RawEmoji
  , disabled :: Bool
  , customID :: CustomID
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON Button

instance CalamityToJSON' Button where
  toPairs Button {..} =
    [ "style" .= style
    , "label" .?= label
    , "emoji" .?= emoji
    , "disabled" .= disabled
    , "custom_id" .= customID
    , "type" .= ButtonType
    ]

$(deriveTextShow ''Button)
$(makeFieldLabelsNoPrefix ''Button)


instance Aeson.FromJSON Button where
  parseJSON = Aeson.withObject "Components.Button" $ \v ->
    Button
      <$> v .: "style"
      <*> v .:? "label"
      <*> v .:? "emoji"
      <*> v .:? "disabled" .!= False
      <*> v .: "custom_id"

data LinkButton = LinkButton
  { style :: ButtonStyle
  , label :: Maybe T.Text
  , emoji :: Maybe RawEmoji
  , url :: T.Text
  , disabled :: Bool
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON LinkButton

instance CalamityToJSON' LinkButton where
  toPairs LinkButton {..} =
      [ "style" .= style
      , "label" .?= label
      , "emoji" .?= emoji
      , "url" .= url
      , "disabled" .= disabled
      , "type" .= ButtonType
      ]

instance Aeson.FromJSON LinkButton where
  parseJSON = Aeson.withObject "Components.Linkbutton" $ \v ->
    LinkButton
      <$> v .: "style"
      <*> v .:? "label"
      <*> v .:? "emoji"
      <*> v .: "url"
      <*> v .:? "disabled" .!= False

$(deriveTextShow ''LinkButton)
$(makeFieldLabelsNoPrefix ''LinkButton)

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

data SelectOption = SelectOption
  { label :: T.Text
  , value :: T.Text
  , description :: Maybe T.Text
  , emoji :: Maybe RawEmoji
  , default_ :: Bool
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON SelectOption

instance CalamityToJSON' SelectOption where
  toPairs SelectOption {..} =
      [ "label" .= label
      , "value" .= value
      , "description" .?= description
      , "emoji" .?= emoji
      , "default" .= default_
      ]

instance Aeson.FromJSON SelectOption where
  parseJSON = Aeson.withObject "Components.SelectOption" $ \v ->
    SelectOption
      <$> v .: "label"
      <*> v .: "value"
      <*> v .:? "description"
      <*> v .:? "emoji"
      <*> v .:? "default" .!= False

$(deriveTextShow ''SelectOption)
$(makeFieldLabelsNoPrefix ''SelectOption)

data Select = Select
  { options :: [SelectOption]
  , placeholder :: Maybe T.Text
  , minValues :: Maybe Int
  , maxValues :: Maybe Int
  , disabled :: Bool
  , customID :: CustomID
  }
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON Select

instance CalamityToJSON' Select where
  toPairs Select {..} =
      [ "options" .= options
      , "placeholder" .?= placeholder
      , "min_values" .?= minValues
      , "max_values" .?= maxValues
      , "disabled" .= disabled
      , "custom_id" .= customID
      , "type" .= SelectType
      ]

instance Aeson.FromJSON Select where
  parseJSON = Aeson.withObject "Components.Select" $ \v ->
    Select
      <$> v .: "options"
      <*> v .:? "placeholder"
      <*> v .:? "min_values"
      <*> v .:? "max_values"
      <*> v .:? "disabled" .!= False
      <*> v .: "custom_id"

$(deriveTextShow ''Select)
$(makeFieldLabelsNoPrefix ''Select)

select :: [SelectOption] -> CustomID -> Select
select o = Select o Nothing Nothing Nothing False

sopt ::
  -- | Label
  T.Text ->
  -- | Value
  T.Text ->
  SelectOption
sopt l v = SelectOption l v Nothing Nothing False

data TextInputStyle
  = TextInputShort
  | TextInputParagraph
  deriving (Show)

$(deriveTextShow ''TextInputStyle)

instance Aeson.ToJSON TextInputStyle where
  toJSON t = Aeson.toJSON @Int $ case t of
    TextInputShort -> 1
    TextInputParagraph -> 2
  toEncoding t = Aeson.toEncoding @Int $ case t of
    TextInputShort -> 1
    TextInputParagraph -> 2

instance Aeson.FromJSON TextInputStyle where
  parseJSON = Aeson.withScientific "Components.TextInputStyle" $ \n -> case toBoundedInteger @Int n of
    Just v -> case v of
      1 -> pure TextInputShort
      2 -> pure TextInputParagraph
      _ -> fail $ "Invalid TextInputStyle: " <> show n
    Nothing -> fail $ "Invalid TextInputStyle: " <> show n

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
  deriving (Show)
  deriving (Aeson.ToJSON) via CalamityToJSON TextInput

instance CalamityToJSON' TextInput where
  toPairs TextInput {..} =
      [ "style" .= style
      , "label" .= label
      , "min_length" .= minLength
      , "max_length" .= maxLength
      , "required" .= required
      , "value" .= value
      , "placeholder" .= placeholder
      , "custom_id" .= customID
      , "type" .= TextInputType
      ]

instance Aeson.FromJSON TextInput where
  parseJSON = Aeson.withObject "Components.TextInput" $ \v ->
    TextInput
      <$> v .: "style"
      <*> v .: "label"
      <*> v .:? "min_length"
      <*> v .:? "max_length"
      <*> v .:? "required" .!= False
      <*> v .:? "value"
      <*> v .:? "placeholder"
      <*> v .: "custom_id"

$(deriveTextShow ''TextInput)
$(makeFieldLabelsNoPrefix ''TextInput)

textInput ::
  TextInputStyle ->
  -- | Label
  T.Text ->
  CustomID ->
  TextInput
textInput s l = TextInput s l Nothing Nothing True Nothing Nothing

data Component
  = ActionRow' [Component]
  | Button' Button
  | LinkButton' LinkButton
  | Select' Select
  | TextInput' TextInput
  deriving (Show)

$(deriveTextShow ''Component)

instance Aeson.ToJSON Component where
  toJSON t =
    case t of
      ActionRow' xs -> Aeson.object . catMaybes $ ["components" .= xs, "type" .= ActionRowType]
      Button' b -> Aeson.toJSON b
      LinkButton' lb -> Aeson.toJSON lb
      Select' s -> Aeson.toJSON s
      TextInput' ti -> Aeson.toJSON ti

  toEncoding t =
    case t of
      ActionRow' xs -> Aeson.pairs . mconcat . catMaybes $ ["components" .= xs, "type" .= ActionRowType]
      Button' b -> Aeson.toEncoding b
      LinkButton' lb -> Aeson.toEncoding lb
      Select' s -> Aeson.toEncoding s
      TextInput' ti -> Aeson.toEncoding ti

instance Aeson.FromJSON Component where
  parseJSON = Aeson.withObject "Component" $ \v -> do
    type_ :: ComponentType <- v .: "type"

    case type_ of
      ActionRowType -> ActionRow' <$> v .: "components"
      ButtonType -> do
        cid :: Maybe CustomID <- v .:? "custom_id"
        url :: Maybe T.Text <- v .:? "url"
        case (cid, url) of
          (Just _, _) -> Button' <$> Aeson.parseJSON (Aeson.Object v)
          (_, Just _) -> LinkButton' <$> Aeson.parseJSON (Aeson.Object v)
          _ -> fail $ "Impossible button: " <> show v
      SelectType -> Select' <$> Aeson.parseJSON (Aeson.Object v)
      TextInputType -> TextInput' <$> Aeson.parseJSON (Aeson.Object v)

componentType :: Component -> ComponentType
componentType (ActionRow' _) = ActionRowType
componentType (Button' _) = ButtonType
componentType (LinkButton' _) = ButtonType
componentType (Select' _) = SelectType
componentType (TextInput' _) = TextInputType
