{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal utilities and instances
module CalamityCommands.Internal.Utils (
  -- whileMFinalIO
  -- , untilJustFinalIO
  whenJust,
  whenM,
  unlessM,
  lastMaybe,
  leftToMaybe,
  rightToMaybe,
  justToEither,
  mapLeft,
  (<<$>>),
  (<<*>>),
  (<.>),
) where

-- import           CalamityCommands.Internal.RunIntoIO

import Control.Applicative

import Data.Semigroup (Last (..))

-- import qualified Polysemy              as P

-- -- | Like whileM, but stateful effects are not preserved to mitigate memory leaks
-- --
-- -- This means Polysemy.Error won't work to break the loop, etc.
-- -- Instead, Error/Alternative will just result in the loop quitting.
-- whileMFinalIO :: P.Member (P.Final IO) r => P.Sem r Bool -> P.Sem r ()
-- whileMFinalIO action = do
--   action' <- runSemToIO action
--   P.embedFinal $ go action'
--   where go action' = do
--           r <- action'
--           case r of
--             Just True ->
--               go action'
--             _ ->
--               pure ()

-- -- | Like untilJust, but stateful effects are not preserved to mitigate memory leaks
-- --
-- -- This means Polysemy.Error won't work to break the loop, etc.
-- -- Instead, Error/Alternative will just result in another loop.
-- untilJustFinalIO :: P.Member (P.Final IO) r => P.Sem r (Maybe a) -> P.Sem r a
-- untilJustFinalIO action = do
--   action' <- runSemToIO action
--   P.embedFinal $ go action'
--   where go action' = do
--           r <- action'
--           case r of
--             Just (Just a) ->
--               pure a
--             _ ->
--               go action'

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (pure ())

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= \case
    True -> m
    False -> pure ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM = whenM . (not <$>)

lastMaybe :: Maybe a -> Maybe a -> Maybe a
lastMaybe l r = getLast <$> fmap Last l <> fmap Last r

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left (f e)
mapLeft _ (Right x) = Right x

leftToMaybe :: Either e a -> Maybe e
leftToMaybe (Left x) = Just x
leftToMaybe _ = Nothing

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe _ = Nothing

justToEither :: Maybe e -> Either e ()
justToEither (Just x) = Left x
justToEither _ = Right ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

infixl 4 <<*>>

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
(<.>) f g x = f <$> g x

infixl 4 <.>
