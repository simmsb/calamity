-- | Invite endpoints
module Calamity.HTTP.Invite
    ( InviteRequest(..) ) where

import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Calamity.Types.Model.Guild

import           Control.Lens                   hiding ( (.=) )

import           Data.Text                      ( Text )

import           Network.Wreq

import           TextShow

data InviteRequest a where
  GetInvite    :: Text -> InviteRequest Invite
  DeleteInvite :: Text -> InviteRequest ()

baseRoute :: RouteBuilder _
baseRoute = mkRouteBuilder // S "invites"

instance Request (InviteRequest a) a where
  toRoute (GetInvite c) = baseRoute // S c
    & buildRoute
  toRoute (DeleteInvite c) = baseRoute // S c
    & buildRoute

  toAction (GetInvite _) = getWithP (param "with_counts" .~ [showt True])
  toAction (DeleteInvite _) = deleteWith
