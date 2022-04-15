-- | Calamity Interaction views
module Calamity.Interactions (
  module Calamity.Interactions.View,
  module Calamity.Interactions.Utils,
  module Calamity.Interactions.Eff,

  -- * Interactions and Views
  -- $viewDocs
) where

import Calamity.Interactions.Eff
import Calamity.Interactions.Utils
import Calamity.Interactions.View

{- $viewDocs

 This module contains functions and types related to handling discord interactions.

 The data models for components can be found in "Calamity.Types.Model.Channel.Component"

 ==== Examples

 Displaying a 'View'

 @
 {-# LANGUAGE ApplicativeDo #-}

 let view = 'row' $ do
       a <- 'button' 'Calamity.Types.Model.Channel.Component.ButtonPrimary' "defer"
       b <- 'button' 'Calamity.Types.Model.Channel.Component.ButtonPrimary' "deferEph"
       c <- 'button' 'Calamity.Types.Model.Channel.Component.ButtonPrimary' "deferComp"
       d <- 'button' 'Calamity.Types.Model.Channel.Component.ButtonPrimary' "modal"
       pure (a, b, c, d)

     modalView = do
       a <- 'textInput' 'Calamity.Types.Model.Channel.Component.TextInputShort' "a"
       b <- 'textInput' 'Calamity.Types.Model.Channel.Component.TextInputParagraph' "b"
       pure (a, b)
 in 'runView' view ('Calamity.tell' ctx) $ \\(a, b, c, d) -> do
      'Control.Monad.when' a $ do
        'Control.Monad.void' 'defer'
        'Polysemy.embed' $ threadDelay 1000000
        'Control.Monad.void' $ 'followUp' \@'Data.Text.Text' "lol"

      'Control.Monad.when' b $ do
        'Control.Monad.void' 'deferEphemeral'
        'Polysemy.embed' $ threadDelay 1000000
        'Control.Monad.void' $ 'followUpEphemeral' \@'Data.Text.Text' "lol"

      'Control.Monad.when' c $ do
        'Control.Monad.void' 'deferComponent'
        'Polysemy.embed' $ threadDelay 1000000
        'Control.Monad.void' $ 'followUp' \@'Data.Text.Text' "lol"

      'Control.Monad.when' d $ do
        'Control.Monad.void' . 'Polysemy.Async.async' $ do
          'runView' modalView ('Control.Monad.void' . 'pushModal' "lol") $ \(a, b) -> do
            'Polysemy.embed' $ print (a, b)
            'Control.Monad.void' $ 'respond' ("Thanks: " <> a <> " " <> b)
            'endView' ()

      pure ()
 @
-}
