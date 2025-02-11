module Demo.Handlers where

import Data.ByteString.Lazy as LBS

import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic (ToServant)

import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H

import Api.Types
import Demo.RouteDef (DemoTopRoutes (..), SearchContent (..))

import Demo.DemoPage (demoPage, demoSearch)
import Demo.MockData (projectList)

demoHandlers :: ToServant DemoTopRoutes (AsServerT EasyVerseApp)
demoHandlers =
  genericServerT $ DemoTopRoutes {
    demoWs = testWS
    , demoSrch = demoSearchHandler
  }

demoSearchHandler :: SearchContent -> EasyVerseApp Html
demoSearchHandler needle = do
  pure . Html . LBS.toStrict . H.renderHtml $ demoSearch projectList needle.search

testWS :: EasyVerseApp Html
testWS = do
  pure . Html . LBS.toStrict . H.renderHtml $ demoPage "First Test Page WS." []
