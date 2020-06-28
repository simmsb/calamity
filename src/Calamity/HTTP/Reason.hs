-- | A wrapper request that adds a reson to a request
module Calamity.HTTP.Reason
    ( Reason(..)
    , reason ) where

import           Calamity.HTTP.Internal.Request

import           Control.Lens                   hiding ( (.=) )

import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )

import           GHC.Generics

import           Network.Wreq.Lens

data Reason a = Reason a Text
  deriving ( Show, Eq, Generic )

-- | Attach a reason to a request
reason :: Request a => Text -> a -> Reason a
reason = flip Reason

instance Request a => Request (Reason a) where
  type Result (Reason a) = Result a

  route (Reason a _) = route a

  action (Reason a r) = action a . (header "X-Audit-Log-Reason" .~ [encodeUtf8 r])
