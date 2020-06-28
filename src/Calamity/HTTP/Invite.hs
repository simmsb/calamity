-- | Invite endpoints
module Calamity.HTTP.Invite
    ( InviteRequest(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Types.Model.Guild

import           Control.Lens                   hiding ( (.=) )

import           Data.Text                      ( Text )

import           Network.Wreq.Lens
import           Network.Wreq.Session

import           TextShow

data InviteRequest a where
  GetInvite    :: Text -> InviteRequest Invite
  DeleteInvite :: Text -> InviteRequest ()

baseRoute :: RouteBuilder _
baseRoute = mkRouteBuilder // S "invites"

instance Request (InviteRequest a) where
  type Result (InviteRequest a) = a

  route (GetInvite c) = baseRoute // S c
    & buildRoute
  route (DeleteInvite c) = baseRoute // S c
    & buildRoute

  action (GetInvite _) = getWithP (param "with_counts" .~ [showt True])
  action (DeleteInvite _) = deleteWith
