{-# LANGUAGE TemplateHaskell #-}

module Calamity.Interactions.View (
  ViewEff (..),
  endView,
  replaceView,
  getSendResponse,
  View,
  row,
  runView,
  runViewInstance,
  button,
  button',
  select,
  select',
  textInput,
  textInput',
  deleteInitialMsg,
  instantiateView,
) where

import Calamity.Client.Client (react)
import Calamity.Client.Types (BotC, EventType (InteractionEvt))
import Calamity.HTTP.Channel (ChannelRequest (DeleteMessage))
import Calamity.HTTP.Internal.Ratelimit (RatelimitEff)
import Calamity.HTTP.Internal.Request (invoke)
import Calamity.Interactions.Eff (InteractionEff (..))
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import Calamity.Types.Model.Channel.Component (CustomID)
import qualified Calamity.Types.Model.Channel.Component as C
import Calamity.Types.Model.Channel.Message (Message)
import Calamity.Types.Model.Interaction
import Calamity.Types.TokenEff (TokenEff)
import qualified Control.Concurrent.STM as STM
import Optics
import Control.Monad (guard, void)
import qualified Data.Aeson as Aeson
import qualified Data.List
import qualified Data.Set as S
import Data.Text (Text)
import qualified GHC.TypeLits as E
import qualified Polysemy as P
import qualified Polysemy.Resource as P
import qualified Polysemy.State as P
import System.Random
import Data.Aeson ((.:?), (.:))
import Data.Aeson.Optics

data ViewComponent a = ViewComponent
  { component :: C.Component
  , parse :: Interaction -> ExtractResult a
  }

instance Functor ViewComponent where
  fmap f ViewComponent {component, parse} = ViewComponent {component, parse = fmap f . parse}

{- | A view containing one or more components

 This has an applicative interface to allow for easy composition of
 components.
-}
data View a
  = NilView a
  | SingView (forall g. RandomGen g => g -> (ViewComponent a, g))
  | RowView (View a)
  | forall x. MultView (View (x -> a)) (View x)

{- | Convert a 'View' such that it renders as a row.

 Note: nested rows are not allowed by discord, along with further restrictions
 listed here:
 https://discord.com/developers/docs/interactions/message-components
-}
row :: View a -> View a
row = RowView

instance Functor View where
  fmap f (NilView x) = NilView (f x)
  fmap f (SingView x) = SingView (\r -> let (x', r') = x r in (f <$> x', r'))
  fmap f (RowView x) = RowView (fmap f x)
  fmap f (MultView x y) = MultView (fmap (f .) x) y

instance Applicative View where
  pure = NilView
  (<*>) = MultView

type MonadViewMessage =
  'E.ShowType View
    'E.:<>: 'E.Text " doesn't have a Monad instance as we need to be able to inspect the contained components"
    'E.:$$: 'E.Text "If you haven't already, enable ApplicativeDo"
    'E.:$$: 'E.Text "Also, make sure you use lazy patterns: ~(a, b) instead of (a, b)"
    'E.:$$: 'E.Text "Refer to https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/applicative_do.html"

instance E.TypeError MonadViewMessage => Monad View where
  (>>=) = undefined -- unreachable

data ExtractOkType
  = -- | At least one value has been extracted
    SomeExtracted
  | -- | No values have been extracted, we shouldn't trigger the callback
    NoneExtracted
  deriving (Show)

instance Semigroup ExtractOkType where
  SomeExtracted <> _ = SomeExtracted
  _ <> SomeExtracted = SomeExtracted
  _ <> _ = NoneExtracted

data ExtractResult a
  = -- | Extraction succeeded in some way
    ExtractOk ExtractOkType a
  | -- | Bail out from parsing this interaction for the current view
    ExtractFail
  deriving (Show, Functor)

instance Applicative ExtractResult where
  pure = ExtractOk SomeExtracted

  ExtractOk ta f <*> ExtractOk tb x = ExtractOk (ta <> tb) $ f x
  _ <*> _ = ExtractFail

data ViewInstance a = ViewInstance
  { -- customIDS :: Set C.CustomID ,
    extract :: Interaction -> ExtractResult a
  , rendered :: [C.Component]
  }

data ViewEff ret inp sendResp m a where
  -- | Mark the view as finished and set the return value.
  --
  -- This doesn't trigger the immediate exit from the code it is used in, the
  -- view will exit before it would wait for the next event.
  EndView :: ret -> ViewEff ret inp sendResp m ()
  -- | Given a view and a way to display the rendered view to discord, show the
  -- view and start tracking the new view
  --
  -- This works for both message components and modals
  ReplaceView :: View inp -> ([C.Component] -> m ()) -> ViewEff ret inp sendResp m ()
  -- | Get the result of the action that sent a value
  GetSendResponse :: ViewEff ret inp sendResp m sendResp

P.makeSem ''ViewEff

extractCustomID :: Interaction -> Maybe CustomID
extractCustomID Interaction {data_, type_} = do
  guard $ type_ == MessageComponentType
  data' <- data_
  data' ^. #customID

guardComponentType :: Interaction -> C.ComponentType -> Maybe ()
guardComponentType Interaction {data_} expected = do
  data' <- data_
  guard $ data' ^. #componentType == Just expected

extractOkFromMaybe :: Maybe a -> ExtractResult (Maybe a)
extractOkFromMaybe (Just a) = ExtractOk SomeExtracted (Just a)
extractOkFromMaybe Nothing = ExtractOk NoneExtracted Nothing

extractOkFromBool :: Bool -> ExtractResult Bool
extractOkFromBool True = ExtractOk SomeExtracted True
extractOkFromBool False = ExtractOk NoneExtracted False

{- | Construct a 'View' containing a 'C.Button' with the given style and label

 Other fields of 'C.Button' default to 'Nothing'
-}
button :: C.ButtonStyle -> Text -> View Bool
button s l = button' ((#style .~ s) . (#label ?~ l))

{- | Construct a 'View' containing a 'C.Button', then modify the component with
   the passed mapping function

 The 'C.Button' passed to the mapping function will have a style of
 'C.ButtonPrimary', other fields will be 'Nothing'
-}
button' :: (C.Button -> C.Button) -> View Bool
button' f = SingView $ \g ->
  let (cid, g') = uniform g
      comp = C.Button' . f $ C.button C.ButtonPrimary cid
      parse' int =
        Just True
          == ( do
                customID <- extractCustomID int
                guardComponentType int C.ButtonType
                pure $ customID == cid
             )
      parse = extractOkFromBool . parse'
   in (ViewComponent comp parse, g')

{- | Construct a 'View' containing a 'C.Select' with the given list of values

 Each element of the passed options list is used as both the display
 'C.SelectOption.label' and 'C.SelectOption.value', use 'select'' if you
 desire more control
-}
select :: [Text] -> View (Maybe Text)
select opts = ensureOne <$> select' (map (\x -> C.sopt x x) opts) Prelude.id
  where
    ensureOne :: Maybe [Text] -> Maybe Text
    ensureOne mx = do
      o <- mx
      guard $ length o == 1
      case o of
        [x] -> Just x
        _ -> Nothing

{- | Construct a 'View' containing a 'C.Select' with the given options, then
 modify the component with the passed mapping function
-}
select' :: [C.SelectOption] -> (C.Select -> C.Select) -> View (Maybe [Text])
select' opts f = SingView $ \g ->
  let (cid, g') = uniform g
      comp = f $ C.select opts cid
      finalValues = S.fromList $ comp ^.. #options % traversed % #value
      parse int = extractOkFromMaybe $ do
        customID <- extractCustomID int
        guard $ customID == cid
        guardComponentType int C.SelectType
        values <- int ^? #data_ % _Just % #values % _Just
        let values' = S.fromList values
        guard $ S.isSubsetOf values' finalValues
        pure values
   in (ViewComponent (C.Select' comp) parse, g')

data TextInputDecoded = TextInputDecoded
  { value :: Maybe Text
  , customID :: CustomID
  }
  deriving (Show)

instance Aeson.FromJSON TextInputDecoded where
  parseJSON = Aeson.withObject "TextInputDecoded" $ \v ->
    TextInputDecoded
      <$> v .:? "value"
      <*> v .: "custom_id"

parseTextInput :: CustomID -> Interaction -> ExtractResult (Maybe Text)
parseTextInput cid int = extractOkFromMaybe $ do
  components <- int ^? #data_ % _Just % #components
  -- currently, each text input is a singleton actionrow containing a single textinput component

  let textInputs = components ^.. traversed % traversed % key "components" % _Array % traversed
      inputs' :: Aeson.Result [TextInputDecoded] = traverse Aeson.fromJSON textInputs

  inputs <- case inputs' of
    Aeson.Success x -> pure x
    Aeson.Error _ -> Nothing

  thisValue <- Data.List.find ((== cid) . (^. #customID)) inputs

  thisValue ^. #value

{- | Construct a 'View' containing a 'C.TextInput' with the given style and label

 All other fields of 'C.TextInput' default to 'Nothing'

 This view ensures that a value was passed for an input
-}
textInput ::
  C.TextInputStyle ->
  -- | Label
  Text ->
  View Text
textInput s l = SingView $ \g ->
  let (cid, g') = uniform g
      comp = C.TextInput' $ C.textInput s l cid
      parse = ensure <$> parseTextInput cid
   in (ViewComponent comp parse, g')
  where
    ensure (ExtractOk v (Just x)) = ExtractOk v x
    ensure _ = ExtractFail

{- | Construct a 'View' containing a 'C.TextInput' with the given style and label,
   then modify the component with the passed mapping function
-}
textInput' ::
  C.TextInputStyle ->
  -- | Label
  Text ->
  (C.TextInput -> C.TextInput) ->
  View (Maybe Text)
textInput' s l f = SingView $ \g ->
  let (cid, g') = uniform g
      comp = C.TextInput' . f $ C.textInput s l cid
      parse = parseTextInput cid
   in (ViewComponent comp parse, g')

-- componentIDS :: C.Component -> S.Set CustomID
-- componentIDS (C.ActionRow' coms) = S.unions $ map componentIDS coms
-- componentIDS (C.Button' C.Button {customID}) = S.singleton customID
-- componentIDS (C.LinkButton' _) = S.empty
-- componentIDS (C.Select' C.Select {customID}) = S.singleton customID
-- componentIDS (C.TextInput' C.TextInput {customID}) = S.singleton customID

-- | Generate a 'ViewInstance' of a 'View' by filling in 'CustomID's with random values
instantiateView :: RandomGen g => g -> View a -> (ViewInstance a, g)
instantiateView g v =
  case v of
    NilView x -> (ViewInstance (const $ ExtractOk SomeExtracted x) [], g)
    SingView f ->
      let (ViewComponent c p, g') = f g
          i = ViewInstance p [c]
       in (i, g')
    RowView x ->
      let (v'@ViewInstance {rendered}, g') = instantiateView g x
       in (v' {rendered = [C.ActionRow' rendered]}, g')
    MultView a b ->
      let (ViewInstance ia ra, g') = instantiateView g a
          (ViewInstance ib rb, g'') = instantiateView g' b
          inv i = ia i <*> ib i
       in (ViewInstance inv (ra <> rb), g'')

-- | Delete the initial message containing components
deleteInitialMsg :: (BotC r, P.Member (ViewEff a inp (Either e Message)) r) => P.Sem r ()
deleteInitialMsg = do
  ini <- getSendResponse
  case ini of
    Right m ->
      void . invoke $ DeleteMessage m m
    Left _ -> pure ()

-- | Run a 'View', returning the value passed to 'endView'
--
-- This function will not return until 'endView' is used inside the view.
-- If you want it to run in the background, consider using "Polysemy.Async".
--
-- This is async exception safe, you can use libraries such as
-- [polysemy-conc](https://hackage.haskell.org/package/polysemy-conc) to stop
-- views after a timeout.
runView ::
  BotC r =>
  -- | The initial view to render
  View inp ->
  -- | A function to send the rendered view (i.e. as a message or a modal)
  ([C.Component] -> P.Sem r sendResp) ->
  -- | Your callback effect.
  --
  -- local state semantics are preserved between calls here, you can keep state around
  (inp -> P.Sem (InteractionEff ': ViewEff a inp sendResp ': r) ()) ->
  P.Sem r a
runView v sendRendered m = do
  inst@ViewInstance {rendered} <- getStdRandom (`instantiateView` v)
  r <- sendRendered rendered
  runViewInstance r inst m

{- | Run a prerendered 'View', returning the value passed to 'endView'

 This function won't send the view, you should do that yourself
-}
runViewInstance ::
  BotC r =>
  -- | An initial value to act as the value of @GetSendResponse@
  --
  -- If you just sent a message, probably pass that
  sendResp ->
  -- | The initial view to run
  ViewInstance inp ->
  -- | Your callback effect.
  --
  -- In here you get access to the 'InteractionEff' and 'ViewEff' effects.
  --
  -- local state semantics are preserved between calls here, you can keep state around
  (inp -> P.Sem (InteractionEff ': ViewEff a inp sendResp ': r) ()) ->
  P.Sem r a
runViewInstance initSendResp inst m = P.resourceToIOFinal $ do
  eventIn <- P.embed STM.newTQueueIO

  P.bracket
    (P.raise $ react @'InteractionEvt (P.embed . sender eventIn))
    P.raise
    ( \_ -> do
        P.raise $ innerLoop initSendResp inst eventIn m
    )
  where
    interpretInteraction ::
      forall r.
      Interaction ->
      P.Sem (InteractionEff ': r) () ->
      P.Sem r ()
    interpretInteraction int =
      P.interpret
        ( \case
            GetInteraction -> pure int
        )

    interpretView ::
      forall r ret inp sendResp.
      P.Member (P.Embed IO) r =>
      P.Sem (ViewEff ret inp sendResp ': r) () ->
      P.Sem (P.State (Maybe ret, ViewInstance inp, sendResp) ': r) ()
    interpretView =
      P.reinterpretH
        ( \case
            EndView x -> P.modify' (_1 ?~ x) >>= P.pureT
            ReplaceView v m -> do
              inst@ViewInstance {rendered} <- P.embed $ getStdRandom (`instantiateView` v)
              P.modify' (_2 .~ inst)
              P.runTSimple $ m rendered
            GetSendResponse -> P.gets (^. _3) >>= P.pureT
        )

    innerLoop ::
      forall r ret inp sendResp.
      P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r =>
      sendResp ->
      ViewInstance inp ->
      STM.TQueue Interaction ->
      (inp -> P.Sem (InteractionEff ': ViewEff ret inp sendResp ': r) ()) ->
      P.Sem r ret
    innerLoop initialSendResp initialInst inChan m = P.evalState (Nothing, initialInst, initialSendResp) innerLoop'
      where
        innerLoop' :: P.Sem (P.State (Maybe ret, ViewInstance inp, sendResp) ': r) ret
        innerLoop' = do
          (s, ViewInstance {extract}, _) <- P.get
          case s of
            Just x -> pure x
            Nothing -> do
              int <- P.embed $ STM.atomically (STM.readTQueue inChan)
              case extract int of
                ExtractOk SomeExtracted x -> interpretView $ interpretInteraction int (m x)
                _ -> pure ()

              -- if Just True == ((`S.member` customIDS) <$> extractCustomID int)
              --   then case extract int of
              --     ExtractOk SomeExtracted x -> interpretView $ interpretInteraction int (m x)
              --     _ -> pure ()
              --   else pure ()
              innerLoop'

    sender :: STM.TQueue Interaction -> Interaction -> IO ()
    sender eventIn int = STM.atomically $ STM.writeTQueue eventIn int

$(makeFieldLabelsNoPrefix ''TextInputDecoded)
