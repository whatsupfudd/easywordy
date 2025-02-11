module Routing.TopHandlers where

import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic (ToServant)

import Api.Types (EasyVerseApp)
import Routing.TopDef (TopRoutes (..))

import WordPress.Handlers (wordpressHandlers)
import Wapp.Handlers (wappHandlers)
import Demo.Handlers (demoHandlers)
-- Old stuff:
import Api.Handlers (anonHandlers, authHandlers)

serverApiT :: ToServant TopRoutes (AsServerT EasyVerseApp)
serverApiT = genericServerT $ TopRoutes {
    wordpress = wordpressHandlers
    , wapp = wappHandlers
    , demo = demoHandlers
    -- Old stuff:
    , anonymous = anonHandlers
    , authenticated = authHandlers
  }
