{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Routing.TopDef where

import GHC.Generics (Generic)
import Servant.API ((:>), ReqBody, JSON, Capture, Post, Get)
import Servant.API.Generic ((:-), ToServantApi)
import Servant.Auth.Server (Auth, JWT, BasicAuth)

import Wapp.Internal.WordPress.RouteDef (WpTopRoutes)
import Wapp.RouteDef (WappTopRoutes)
import Demo.RouteDef (DemoTopRoutes)

-- Old stuff:
import qualified Api.Types as Api


data TopRoutes route = TopRoutes {
     wordpress :: route :- ToServantApi WpTopRoutes
    , wapp :: route :- ToServantApi WappTopRoutes
    , demo :: route :- ToServantApi DemoTopRoutes
    -- Old stuff:
    , anonymous :: route :- ToServantApi Api.AnonymousRoutes
    , authenticated :: route :- Auth '[JWT, BasicAuth] Api.ClientInfo :> ToServantApi Api.AuthenticatedRoutes
  }
  deriving stock (Generic)

