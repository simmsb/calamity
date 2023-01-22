-- | A wrapper request that adds a reson to a request
module Calamity.HTTP.Reason (
  Reason (..),
  reason,
) where

import Calamity.HTTP.Internal.Request
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

data Reason a = Reason a Text
  deriving (Show, Eq)

-- | Attach a reason to a request
reason :: Request a => Text -> a -> Reason a
reason = flip Reason

instance Request a => Request (Reason a) where
  type Result (Reason a) = Result a

  route (Reason a _) = route a

  action (Reason a r) u o = action a u (o <> header "X-Audit-Log-Reason" (encodeUtf8 r))
