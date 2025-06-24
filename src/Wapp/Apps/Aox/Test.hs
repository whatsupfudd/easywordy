module Wapp.Apps.Aox.Test where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Mp

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11 as Sv
import qualified Text.Blaze.Svg11.Attributes as SvA
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import qualified Aox.Opers as Aop
import qualified Wapp.AppDef as Wd


getUserMailboxes :: Wd.InternalFunction
getUserMailboxes rtOpts pgDb (jsonParams, content) = do
  rezA <- Aop.getUserMailboxes pgDb "hugo_akra"
  case rezA of
    Left err ->
      let
        response = renderHtml $
          H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
            H.toHtml ("@[getUserMailboxes] error: " <> T.pack err :: Text)
      in
      pure . Right $ Wd.BasicFR (response, Nothing)
    Right mailboxes ->
      let
        response = renderHtml $
          H.div H.! A.class_ "text-gray-900 dark:text-gray-100" $
            H.div H.! A.class_ "bg-gray-50 dark:bg-gray-900 p-4 md:ml-64 lg:mr-16 min-h-full pt-20" $
              showMailbox True mailboxes
      in
      -- putStrLn $ "@[getUserMailboxes] mailboxes : " <> show mailboxes
      pure . Right $ Wd.BasicFR (response, Nothing)


showMailbox :: Bool -> Aop.MapMailbox -> H.Html
showMailbox visible mailboxes =
  H.ul $
    mapM_ (\mb -> H.li H.! A.class_ "px-2 hover:bg-secondary-100" $
        H.li $ do
          H.a H.! A.href "#collapseThree" H.! A.class_ "flex items-center px-2 hover:bg-secondary-100 focus:text-primary active:text-primary" $ do
            Sv.svg H.! SvA.fill "none" H.! SvA.viewbox "0 0 24 24" H.! SvA.strokeWidth "2.5" H.! SvA.stroke "currentColor" H.! SvA.class_ "h-4 w-4" $
              Sv.path H.! SvA.strokeLinecap "round" H.! SvA.strokeLinejoin "round" H.! SvA.d "M8.25 4.5l7.5 7.5-7.5 7.5"
            H.toHtml (mb.label <> " (" <> T.pack (show mb.msgCount) <> ")")
          if Mp.null mb.children then
            pure ()
          else
            showMailbox False mb.children
      ) mailboxes

