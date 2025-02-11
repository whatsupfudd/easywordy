{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Wapp.RouteDef where

import Data.Text (Text)
import Data.Map as Mp
import GHC.Generics

import qualified Network.WebSockets as WS

import Servant.API ((:>), Capture, CaptureAll, Get, ReqBody)
import Servant.API.QueryParam (QueryParam')
import Servant.API.Modifiers (Optional, Lenient)
import Servant.API.Generic ((:-), ToServantApi, ToServant)
import Servant.API.WebSocket (WebSocket)
import Servant.Server.Generic (AsServerT, genericServerT)

import Data.Aeson (FromJSON)

import Api.Types (HTML, Html, EasyVerseApp)


newtype WappTopRoutes mode = WappTopRoutes {
    wapp :: mode :- "wbap" :> ToServantApi WappRoutes
 }
 deriving stock (Generic)

data WappRoutes mode = WappRoutes {
    phpTest :: mode :- "index.php" :> QueryParam' '[Optional, Lenient] "p" Int :> Get '[HTML] Html
    , xStatic :: mode :- "xstatic" :> CaptureAll "path" String :> Get '[HTML] Html
    -- TODO: find out how to make the capture optional, giving a Maybe Text.
    , wsStream :: mode :- "stream" :> Capture "sid" Text :> WebSocket
    , rootZN :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)

