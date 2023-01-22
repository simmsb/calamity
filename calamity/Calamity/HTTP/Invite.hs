{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Invite endpoints
module Calamity.HTTP.Invite (InviteRequest (..)) where

import Calamity.HTTP.Internal.Request
import Calamity.HTTP.Internal.Route
import Calamity.Types.Model.Guild
import Data.Function ((&))
import Data.Text (Text)
import Network.HTTP.Req

data InviteRequest a where
  GetInvite :: Text -> InviteRequest Invite
  DeleteInvite :: Text -> InviteRequest ()

baseRoute :: RouteBuilder _
baseRoute = mkRouteBuilder // S "invites"

instance Request (InviteRequest a) where
  type Result (InviteRequest a) = a

  route (GetInvite c) =
    baseRoute // PS @"invite"
      & giveParam @"invite" c
      & buildRoute
  route (DeleteInvite c) =
    baseRoute // PS @"invite"
      & giveParam @"invite" c
      & buildRoute

  action (GetInvite _) = getWithP ("with_counts" =: True)
  action (DeleteInvite _) = deleteWith
