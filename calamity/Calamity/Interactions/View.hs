{-# LANGUAGE TemplateHaskell #-}

module Calamity.Interactions.View (
  ViewEff (..),
  endView,
  replaceView,
  View,
  row,
  runView,
  button,
  button',
  select,
  select',
) where

import Calamity.Client.Client (react)
import Calamity.Client.Types (BotC, EventType (InteractionEvt))
import Calamity.HTTP.Internal.Ratelimit (RatelimitEff)
import Calamity.Interactions.Eff (InteractionEff (..))
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Types.LogEff (LogEff)
import Calamity.Types.Model.Channel.Component (CustomID)
import qualified Calamity.Types.Model.Channel.Component as C
import Calamity.Types.Model.Interaction
import Calamity.Types.TokenEff (TokenEff)
import qualified Control.Concurrent.STM as STM
import Control.Lens ((.~), (?~), (^.), (^..), _1, _2, _Just)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Polysemy as P
import qualified Polysemy.Resource as P
import qualified Polysemy.State as P
import System.Random
import qualified GHC.TypeLits as E

data ViewComponent a = ViewComponent
  { component :: C.Component
  , parse :: Interaction -> a
  }

instance Functor ViewComponent where
  fmap f ViewComponent {component, parse} = ViewComponent {component, parse = f . parse}

data View a
  = NilView a
  | SingView (forall g. RandomGen g => g -> (ViewComponent a, g))
  | RowView (View a)
  | forall x. MultView (View (x -> a)) (View x)

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

type MonadViewMessage
  = 'E.ShowType View 'E.:<>:
    'E.Text "Calamity.View doesn't have a Monad instance as we need to be able to inspect the contained components" 'E.:$$:
    'E.Text "If you haven't already, enable ApplicativeDo" 'E.:$$:
    'E.Text "Also, make sure you use lazy patterns: ~(a, b) instead of (a, b)" 'E.:$$:
    'E.Text "Refer to https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/applicative_do.html"

instance E.TypeError MonadViewMessage => Monad View where
  (>>=) = undefined -- unreachable

data ViewInstance a = ViewInstance
  { customIDS :: Set C.CustomID
  , extract :: Interaction -> a
  , rendered :: [C.Component]
  }

data ViewEff ret inp m a where
  EndView :: ret -> ViewEff ret inp m ()
  -- | Given a view and a way to display the rendered view to discord, show the
  -- view and start tracking the new view
  --
  -- This works for both message components and modals
  ReplaceView :: View inp -> ([C.Component] -> m ()) -> ViewEff ret inp m ()

P.makeSem ''ViewEff

extractCustomID :: Interaction -> Maybe CustomID
extractCustomID Interaction {data_, type_} = do
  guard $ type_ == MessageComponentType
  data' <- data_
  data' ^. #customID

guardComponentType :: Interaction -> C.ComponentType -> _
guardComponentType Interaction {data_} expected = do
  data' <- data_
  guard $ data' ^. #componentType == Just expected

button :: C.ButtonStyle -> Text -> View Bool
button s l = button' ((#style .~ s) . (#label ?~ l))

button' :: (C.Button -> C.Button) -> View Bool
button' f = SingView $ \g ->
  let (cid, g') = uniform g
      comp = C.Button' . f $ C.button C.ButtonPrimary cid
      parse int =
        Just True
          == ( do
                customID <- extractCustomID int
                guardComponentType int C.ButtonType
                pure $ customID == cid
             )
   in (ViewComponent comp parse, g')

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

select' :: [C.SelectOption] -> (C.Select -> C.Select) -> View (Maybe [Text])
select' opts f = SingView $ \g ->
  let (cid, g') = uniform g
      comp = f $ C.select opts cid
      finalValues = S.fromList $ comp ^.. #options . traverse . #value
      parse int = do
        customID <- extractCustomID int
        guard $ customID == cid
        guardComponentType int C.SelectType
        values <- int ^. #data_ . _Just . #values
        let values' = S.fromList values
        guard $ S.isSubsetOf values' finalValues
        pure values
   in (ViewComponent (C.Select' comp) parse, g')

componentIDS :: C.Component -> S.Set CustomID
componentIDS (C.ActionRow' coms) = S.unions $ map componentIDS coms
componentIDS (C.Button' C.Button {customID}) = S.singleton customID
componentIDS (C.LinkButton' _) = S.empty
componentIDS (C.Select' C.Select {customID}) = S.singleton customID
componentIDS (C.TextInput' C.TextInput {customID}) = S.singleton customID

instantiateView :: RandomGen g => g -> View a -> (ViewInstance a, g)
instantiateView g v =
  case v of
    NilView x -> (ViewInstance S.empty (const x) [], g)
    SingView f ->
      let (ViewComponent c p, g') = f g
          i = ViewInstance (componentIDS c) p [c]
       in (i, g')
    RowView x ->
      let (v'@ViewInstance {rendered}, g') = instantiateView g x
       in (v' {rendered = [C.ActionRow' rendered]}, g')
    MultView a b ->
      let (ViewInstance ca ia ra, g') = instantiateView g a
          (ViewInstance cb ib rb, g'') = instantiateView g' b
          cab = S.union ca cb
          inv i = ia i $ ib i
       in (ViewInstance cab inv (ra <> rb), g'')

runView ::
  BotC r =>
  -- | The initial view to render
  View inp ->
  -- | A function to send the rendered view (i.e. as a message or a modal)
  ([C.Component] -> P.Sem r ()) ->
  -- | Your callback effect.
  --
  -- local state semantics are preserved between calls here, you can keep state around
  (inp -> P.Sem (InteractionEff ': ViewEff a inp ': r) ()) ->
  P.Sem r a
runView v sendRendered m = P.resourceToIOFinal $ do
  eventIn <- P.embed STM.newTQueueIO

  P.bracket
    (P.raise $ react @'InteractionEvt (P.embed . sender eventIn))
    P.raise
    ( \_ -> do
        inst@ViewInstance {rendered} <- getStdRandom (`instantiateView` v)
        P.raise $ do
          sendRendered rendered
          innerLoop inst eventIn m
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
      forall r ret inp.
      P.Member (P.Embed IO) r =>
      P.Sem (ViewEff ret inp ': r) () ->
      P.Sem (P.State (Maybe ret, ViewInstance inp) ': r) ()
    interpretView =
      P.reinterpretH
        ( \case
            EndView x -> P.modify' (_1 ?~ x) >>= P.pureT
            ReplaceView v m -> do
              inst@ViewInstance {rendered} <- P.embed $ getStdRandom (`instantiateView` v)
              P.modify' (_2 .~ inst)
              P.runTSimple $ m rendered
        )

    innerLoop ::
      forall r ret inp.
      P.Members '[RatelimitEff, TokenEff, LogEff, MetricEff, P.Embed IO] r =>
      ViewInstance inp ->
      STM.TQueue Interaction ->
      (inp -> P.Sem (InteractionEff ': ViewEff ret inp ': r) ()) ->
      P.Sem r ret
    innerLoop initialInst inChan m = P.evalState (Nothing, initialInst) innerLoop'
      where
        innerLoop' :: P.Sem (P.State (Maybe ret, ViewInstance inp) ': r) ret
        innerLoop' = do
          (s, ViewInstance {customIDS, extract}) <- P.get
          case s of
            Just x -> pure x
            Nothing -> do
              int <- P.embed $ STM.atomically (STM.readTQueue inChan)
              if Just True == ((`S.member` customIDS) <$> extractCustomID int)
                then interpretView $ interpretInteraction int (m $ extract int)
                else pure ()
              innerLoop'

    sender :: STM.TQueue Interaction -> Interaction -> IO ()
    sender eventIn int = STM.atomically $ STM.writeTQueue eventIn int
