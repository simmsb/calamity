-- | Combined http request stuff
module Calamity.HTTP
    ( module Calamity.HTTP.AuditLog
    , module Calamity.HTTP.Internal.Request
    , module Calamity.HTTP.Channel
    , module Calamity.HTTP.Emoji
    , module Calamity.HTTP.Guild
    , module Calamity.HTTP.Invite
    , module Calamity.HTTP.MiscRoutes
    , module Calamity.HTTP.User
    , module Calamity.HTTP.Reason
    , module Calamity.HTTP.Internal.Types
    , module Calamity.HTTP.Webhook
    -- * HTTP
    -- $httpDocs
    ) where

import           Calamity.HTTP.AuditLog
import           Calamity.HTTP.Channel
import           Calamity.HTTP.Emoji
import           Calamity.HTTP.Guild
import           Calamity.HTTP.Internal.Request ( invoke )
import           Calamity.HTTP.Internal.Types   ( RestError )
import           Calamity.HTTP.Invite
import           Calamity.HTTP.MiscRoutes
import           Calamity.HTTP.Reason
import           Calamity.HTTP.User
import           Calamity.HTTP.Webhook

-- $httpDocs
--
-- This module contains all the http related things
--
--
-- ==== Registered Metrics
--
--     1. Gauge: @"inflight_requests" [route]@
--
--         Keeps track of how many requests are currently in-flight, the @route@
--         parameter will be the route that is currently active.
--
--     2. Counter: @"total_requests" [route]@
--
--         Incremented on every request, the @route@ parameter is the route that
--         the request was made on.
--
--
-- ==== Examples
--
-- Editing a message:
--
-- @
-- 'invoke' $ 'EditMessage' someChannel someMessage ('Just' "new content") 'Nothing'
-- @
