module Wapp.Apps.Scenario.Prez where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified Text.Blaze.Internal as Bli
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as Sa
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Wapp.JSSupport (runElmFunction)
import qualified Options.Runtime as Rt
import qualified Wapp.AppDef as Wd

import qualified Wapp.Apps.Scenario.Presentation.Types as Pt
import qualified Wapp.Apps.Scenario.Presentation.Storage as Ps


browsePrez :: Wd.InternalFunction
browsePrez rtOpts pgDb (jsonParams, content) =
  let
    response = renderHtml $
      H.h2 H.! A.class_ "text-2xl font-bold" $ H.text "Test"
  in
  pure . Right $ Wd.BasicFR (response, Nothing)
