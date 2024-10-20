module Wapp.Router where

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as Bs
import qualified Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Map as Mp

import System.FilePath.Posix ((</>))

import Hasql.Pool (Pool)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X

import Options.Runtime (RunOptions (..), ZbConfig (..))
import Wapp.Opers (getVersions, getFoldersForVersion)
import Control.Lens.Internal.CTypes (Int32)

type RoutingTable = Mp.Map Text RouteLogic
type RouteFunction = RunOptions -> Pool -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)

data RouteLogic =
  ExecFileRL FilePath [ RouteArg ]
  | FunctionRL RouteFunction


data RouteArg =
  IntRA Int32
  | TextRA Text


fakeRoutingInit :: RunOptions -> RoutingTable
fakeRoutingInit rtOpts = do
  Mp.fromList [
      ("dashboard_stats", ExecFileRL "dashboardStats_1.html" [])
      , ("khanban_1", ExecFileRL "khanban_1.html" [])
      , ("inbox_1", ExecFileRL "inbox_1.html" [])
      , ("inbox_compose_1", ExecFileRL "inbox_compose_1.html" [])
      , ("inbox_read_1", ExecFileRL "inbox_read_1.html" [])
      , ("inbox_reply_1", ExecFileRL "inbox_reply_1.html" [])
      , ("ecomm_products_1", ExecFileRL "ecomm_products_1.html" [])
      , ("ecomm_billing_1", ExecFileRL "ecomm_billing_1.html" [])
      , ("ecomm_invoices_1", ExecFileRL "ecomm_invoices_1.html" [])
      , ("users_listing_1", ExecFileRL "users_listing_1.html" [])
      , ("users_profile_1", ExecFileRL "users_profile_1.html" [])
      , ("users_feed_1", ExecFileRL "users_feed_1.html" [])
      , ("users_settings_1", ExecFileRL "users_settings_1.html" [])
      , ("pages_pricing_1", ExecFileRL "pages_pricing_1.html" [])
      , ("pages_maint_1", ExecFileRL "pages_maint_1.html" [])
      , ("pages_404_1", ExecFileRL "pages_404_1.html" [])
      , ("pages_500_1", ExecFileRL "pages_500_1.html" [])
      , ("auth_signin_1", ExecFileRL "auth_signin_1.html" [])
      , ("auth_signup_1", ExecFileRL "auth_signup_1.html" [])
      , ("auth_forgot_1", ExecFileRL "auth_forgot_1.html" [])
      , ("auth_reset_1", ExecFileRL "auth_reset_1.html" [])
      , ("auth_lock_1", ExecFileRL "auth_lock_1.html" [])
      , ("wp_versions_1", FunctionRL $ fetchVersions )
      , ("wp_folders_1", FunctionRL fetchFolders)
    ]


routeRequest :: RunOptions -> Pool -> RoutingTable -> Text -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)
routeRequest rtOpts pgDb routingTable anID argMap =
  case Mp.lookup anID routingTable of
    Nothing -> do
      putStrLn $ "@[receiveStream] templatePath not found: " <> show anID
      pure $ Left "<div id=\"mainContainer\"></div>"
    Just (ExecFileRL templatePath _) -> do
      putStrLn $ "@[receiveStream] templatePath: " <> templatePath
      response <- liftIO $ Bs.readFile (rtOpts.zb.zbRootPath </> templatePath)
      putStrLn $ "@[receiveStream] sending " <> show (Bs.length response) <> " bytes."
      pure $ Right $ "<div id=\"mainContainer\">" <> response <> "</div>"
    Just (FunctionRL fetchFunc) -> do
      fetchFunc rtOpts pgDb argMap


fetchVersions :: RunOptions -> Pool -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)
fetchVersions rtOpts pgDb argMap = do
  rezA <- getVersions pgDb
  case rezA of
    Left err -> pure . Right $ Bs.toStrict . renderHtml $ H.div 
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml err
    Right versions ->
      let
        response =  H.div H.! A.id "mainContainer" $
          H.table H.! A.class_ "text-blue-900 dark:text-white table-auto" $ do
            H.thead $ do
              H.tr $ H.th "ID" >> H.th "Label"
            H.tbody $ do
              mapM_ (\(uid, label) -> H.tr $ do
                  -- hx-target="#mainContainer" hx-swap="outerHtml" hx-headers='{"mid":"wp_versions_1"}'
                  H.td H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml" H.! X.hxHeaders "{\"mid\":\"wp_folders_1\"}" $ H.toHtml uid
                  H.td $ H.toHtml label
                ) versions
      in
      pure . Right . Bs.toStrict . renderHtml $ response

fetchFolders :: RunOptions -> Pool -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)
fetchFolders rtOpts pgDb argMap = do
  rezA <- getFoldersForVersion pgDb 1
  case rezA of
    Left err -> pure . Right $ Bs.toStrict . renderHtml $ H.div 
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml err
    Right versions ->
      let
        response =  H.div H.! A.id "mainContainer" $
          H.table H.! A.class_ "text-blue-900 dark:text-white table-auto" $ do
            H.thead $ do
              H.tr $ H.th "ID" >> H.th "Label"
            H.tbody $ do
              mapM_ (\(uid, label) -> H.tr $ do
                  H.td H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml" H.! X.hxHeaders "{\"mid\":\"wp_files_1\"}" $ H.toHtml uid
                  H.td $ H.toHtml label
                ) versions
      in
      pure . Right . Bs.toStrict . renderHtml $ response
