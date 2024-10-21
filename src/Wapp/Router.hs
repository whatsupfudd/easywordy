module Wapp.Router where

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as Bs
import qualified Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath.Posix ((</>))
import qualified Data.Vector as V
import qualified Data.Binary.Get as Bg
import qualified Numeric as Nm

import Hasql.Pool (Pool)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Htmx as X
import qualified Text.Blaze.Htmx.WebSockets as X
import qualified Text.Blaze.Internal as Bli

import Options.Runtime (RunOptions (..), ZbConfig (..))
import Wapp.Opers (getVersions, getFoldersForVersion, getFilesForFolder, getConstantsForFile, getAstForFile)
import Control.Lens.Internal.CTypes (Int32)
import Data.Word (Word8)

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
      , ("wp_files_1", FunctionRL fetchFiles)
      , ("wp_fileDetails_1", FunctionRL fetchFileDetails)
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
                  H.td H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml" H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_folders_1\", \"args\":\"version=" <> T.pack (show uid) <> "\"}") $ H.toHtml uid
                  H.td $ H.toHtml label
                ) versions
      in
      pure . Right . Bs.toStrict . renderHtml $ response


fetchFolders :: RunOptions -> Pool -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)
fetchFolders rtOpts pgDb argMap = do
  let
    versionID = case Mp.lookup "version" argMap of
      Just (TextRA versionID) -> read . T.unpack $ versionID
      _ -> 1
  rezA <- getFoldersForVersion pgDb versionID
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
                  H.td H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml" H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_files_1\", \"args\":\"folder=" <> T.pack (show uid) <> "\"}") $ H.toHtml uid
                  H.td $ H.toHtml label
                ) versions
      in
      pure . Right . Bs.toStrict . renderHtml $ response


fetchFiles :: RunOptions -> Pool -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)
fetchFiles rtOpts pgDb argMap = do
  let
    folderID = case Mp.lookup "folder" argMap of
      Just (TextRA folderID) -> read . T.unpack $ folderID
      _ -> 1
  rezA <- getFilesForFolder pgDb folderID
  case rezA of
    Left err -> pure . Right $ Bs.toStrict . renderHtml $ H.div
        H.! A.id "mainContainer"
        H.! A.class_ "text-gray-900 dark:text-gray-100" $ H.toHtml err
    Right files ->
      let
        response =  H.div H.! A.id "mainContainer" $
          H.table H.! A.class_ "text-blue-900 dark:text-white table-auto" $ do
            H.thead $ do
              H.tr $ H.th "ID" >> H.th "Label"
            H.tbody $ do
              mapM_ (\(uid, label) -> H.tr $ do
                  H.td H.! X.wsSend "" H.! X.hxTarget "#mainContainer" H.! X.hxSwap "outerHtml" H.! X.hxHeaders (Bli.textValue $ "{\"mid\":\"wp_fileDetails_1\", \"args\":\"file=" <> T.pack (show uid) <> "\"}") $ H.toHtml uid
                  H.td $ H.toHtml label
                ) files
      in
      pure . Right . Bs.toStrict . renderHtml $ response


fetchFileDetails :: RunOptions -> Pool -> Mp.Map Text RouteArg -> IO (Either String Bs.ByteString)
fetchFileDetails rtOpts pgDb argMap = do
  let
    fileID = case Mp.lookup "file" argMap of
      Just (TextRA fileID) -> read . T.unpack $ fileID
      _ -> 1
  rezA <- getAstForFile pgDb fileID
  rezB <- getConstantsForFile pgDb fileID
  case (rezA, rezB) of
    (Left err, _) -> pure . Right $ Bs.toStrict . renderHtml $
        H.div H.! A.id "mainContainer" H.! A.class_ "text-gray-900 dark:text-gray-300" $ H.toHtml err
    (_, Left err) -> pure . Right $ Bs.toStrict . renderHtml $
        H.div H.! A.id "mainContainer" H.! A.class_ "text-gray-900 dark:text-gray-300" $ H.toHtml err
    (Right (Just ast), Right (Just constants)) ->
      let
        derefedAst = printAst ast constants
      in
      pure . Right $ Bs.toStrict . renderHtml $ H.div H.! A.id "mainContainer" $ derefedAst
    _ -> pure . Right $ Bs.toStrict . renderHtml $
      H.div H.! A.id "mainContainer" H.! A.class_ "text-gray-900 dark:text-gray-300" $ H.toHtml ("No AST or Constants" :: Text)


printAst :: Bs.ByteString -> Bs.ByteString -> H.Html
printAst astBs constantsBs =
  let
    (constantVect, nbrConstants, offsets) = buildConstants constantsBs
    astHtml = Bg.runGet (parseAst constantVect) (Bs.fromStrict astBs)
  in
  H.div H.! A.class_ "p-4 text-gray-900 dark:text-gray-300" $ do
        H.toHtml ("AST" <> T.intercalate " " (
            map (\w8 -> T.pack $ Nm.showHex (w8 :: Word8) "")
              (Bs.unpack (Bs.take 10 astBs))
          ))
        H.br
        astHtml
        H.br
        H.toHtml ("Nbr constants:" <> T.pack (show nbrConstants) <> ", length: " <> T.pack (show offsets))
        H.br
        H.toHtml ("\nConstants: " :: Text)
        mapM_ (\(constant, idx) -> do
            H.br
            H.toHtml $ T.pack (show idx) <> ": " <> T.pack (show constant)
          ) (V.zip constantVect (V.enumFromN 0 (fromIntegral nbrConstants)))


buildConstants :: Bs.ByteString -> (V.Vector Bs.ByteString, Int32, [Int32])
buildConstants byteString =
  let
    (nbrConstants, offsets) = Bg.runGet parseConstantHeader (Bs.fromStrict byteString)
    (constants, _) = foldl (\(accum, position) offset ->
          (
            V.snoc accum (Bs.take offset (Bs.drop position byteString))
            , position + offset
          )
        ) (V.empty, 8 + 4 * fromIntegral nbrConstants) (map fromIntegral offsets)
  in
  (constants, nbrConstants, offsets)


parseConstantHeader :: Bg.Get (Int32, [Int32])
parseConstantHeader = do
  nbrConstants <- Bg.getInt32be
  totalLength <- Bg.getInt32be
  offsets <- mapM (const Bg.getInt32be) [1..nbrConstants]
  pure (nbrConstants, offsets)


parseAst :: V.Vector Bs.ByteString -> Bg.Get H.Html
parseAst constantVec = do
  stmt <- Bg.getInt32be
  case stmt of
    1 -> do
      cteID <- Bg.getInt32be
      pure $ H.div $ H.toHtml (T.decodeUtf8 $ "Verbatim " <> constantVec V.! fromIntegral cteID)
    2 -> do
      cteID <- Bg.getInt32be
      pure $ H.div $ H.toHtml (T.decodeUtf8 $ "Comment " <> constantVec V.! fromIntegral cteID)
    3 -> do
      startRow <- Bg.getInt32be
      startCol <- Bg.getInt32be
      endRow <- Bg.getInt32be
      endCol <- Bg.getInt32be
      pure $ H.div $ H.toHtml ("MiscST " <> T.pack (show startRow) <> ":" <> T.pack (show startCol) <> "-" <> T.pack (show endRow) <> ":" <> T.pack (show endCol))
    _ -> pure $ H.div $ H.toHtml ("Unknown statement type " <> T.pack (show stmt))
