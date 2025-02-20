module Wapp.Registry where

import Control.Monad (forM)

import Data.Either (lefts, rights)
import Data.List (intercalate)
import qualified Data.Map as Mp
import Data.Text (Text, pack, unpack)
import Data.UUID (UUID)
import qualified System.Directory as Sd
import qualified System.Environment as Se
import qualified System.FilePath.Posix as Spx
import qualified System.IO.Error as Er

import qualified Data.Yaml as Y

import qualified Options.Runtime as Rt
import WordPress.Functions (fetchVersions, fetchFolders, fetchFiles, fetchFileDetails)
import Chat.Logic (receiveMsg)
import Wapp.Types
import Data.Char (isLetter)


loadAppDefs :: FilePath -> IO (Either String RoutingDictionary)
loadAppDefs aDir = do
  let
    fctResolver = defaultFctResolver
  appDefs <- Sd.listDirectory aDir
  let
    validFiles = filter (\filePath -> Spx.takeExtension filePath == ".yaml" && isLetter (head filePath)) appDefs
  rezA <- forM validFiles $ \filePath ->
    let
      appDefFile = Spx.joinPath [aDir, filePath]
    in do
    rezC <- Y.decodeFileEither appDefFile :: IO (Either Y.ParseException AppDef)
    case rezC of
      Left err ->
        pure . Left $ (filePath, err)
      Right appDef ->
        pure . Right $ appDef
  case lefts rezA of
    [] -> do
      rezB <- forM (rights rezA) $ \appDef ->
        resolveAppDef fctResolver appDef
      case lefts rezB of
        [] ->
          pure . Right $ Mp.fromList $ map (\app -> (app.aid, app)) (rights rezB)
        errs ->
          let
            errMsg = intercalate "\n" $ map show errs
          in do
          putStrLn $ "@[loadAppDefs] Error resolving app defs: " <> errMsg
          return $ Left errMsg
    errs ->
      let
        errMsg = intercalate "\n" $ map (\(filePath, err) -> filePath <> ": " <> show err) errs
      in do
      putStrLn $ "@[loadAppDefs] Error loading app defs: " <> errMsg
      return $ Left errMsg


defaultFctResolver :: FctResolver
defaultFctResolver = FctResolver {
  resolveLib = Right
  , resolveFct = functionResolver
  }


functionResolver :: Mp.Map Text Text -> FunctionDef -> Either String (Text, RouteLogic)
functionResolver libs fctDef =
  case fctDef.action of
    FileVerbatimAD filePath ->
      Right (fctDef.fid, ExecFileRL filePath [])
    TemplateAD filePath ->
      Left "Not implemented"
    FunctionAD pairRef ->
      case pairRef.label of
        "internal.wordpress" ->
          let
            eiInternalFct = case pairRef.ident of
              "fetchVersions" ->
                Right $ FunctionRL $ Internal fetchVersions
              "fetchFolders" ->
                Right $ FunctionRL $ Internal fetchFolders
              "fetchFiles" ->
                Right $ FunctionRL $ Internal fetchFiles
              "fetchFileDetails" ->
                Right $ FunctionRL $ Internal fetchFileDetails
              _ ->
                Left $ "Unknown function: " <> unpack pairRef.ident
          in
            case eiInternalFct of
              Left err ->
                Left err
              Right fct ->
                Right (fctDef.fid, fct)
        "internal.chat" ->
          let
            eiInternalFct = case pairRef.ident of
              "receiveMsg" ->
                Right $ FunctionRL $ Internal receiveMsg
              _ ->
                Left $ "Unknown function: " <> unpack pairRef.ident
          in
            case eiInternalFct of
              Left err ->
                Left err
              Right fct ->
                Right (fctDef.fid, fct)
        aName ->
          case Mp.lookup aName libs of
            Just aPath ->
              Right (fctDef.fid, FunctionRL $ External (aPath, aName, pairRef.ident))
            Nothing ->
              Left $ "Unknown library: " <> unpack aName


resolveAppDef :: FctResolver -> AppDef -> IO (Either String ResolvedApp)
resolveAppDef fctResolver appDef =
  let
    eiLibs = map fctResolver.resolveLib appDef.libs
  in
  pure $ case lefts eiLibs of
    [] ->
      let
        libRefs = rights eiLibs
        libs = Mp.fromList $ map (\pairRef -> (pairRef.label, pairRef.ident)) libRefs
        eiFcts = map (fctResolver.resolveFct libs) appDef.functions
      in
      case lefts eiFcts of
        [] ->
          Right $ ResolvedApp {
              aid = appDef.uid
              , label = appDef.label
              , locales = appDef.locales
              , rootPath = appDef.rootPath
              , libs = libRefs
              , functions = Mp.fromList (rights eiFcts)
            }
        fctErrs ->
          Left $ "Error resolving functions: " <> intercalate "\n" fctErrs
    libErrs ->
      Left $ "Error resolving libs: " <> intercalate "\n" libErrs
      


findRoutingForApp :: Rt.RunOptions -> Text -> Maybe RoutingTable
findRoutingForApp rtOpts anID =
  case anID of
    "00000000-0000-0000-0000-000000000000" -> Just $ fakeZpNsInit rtOpts
    "00000000-0000-0000-0000-000000000001" -> Just $ fakeZ14LInit rtOpts
    _ -> Nothing


fakeZpNsInit :: Rt.RunOptions -> RoutingTable
fakeZpNsInit rtOpts = do
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
      , ("wp_versions_1", FunctionRL $ Internal fetchVersions)
      , ("wp_folders_1", FunctionRL $ Internal fetchFolders)
      , ("wp_files_1", FunctionRL $ Internal fetchFiles)
      , ("wp_fileDetails_1", FunctionRL $ Internal fetchFileDetails)
    ]


fakeZ14LInit :: Rt.RunOptions -> RoutingTable
fakeZ14LInit rtOpts =
  Mp.fromList [
      -- Left side menu items:
      ("l_overview_1", ExecFileRL "z14l/l_overview_1.html" [])
      , ("l_stream_1", ExecFileRL "z14l/l_stream_1.html" [])
      , ("l_sections_pathway_1", ExecFileRL "z14l/l_sections_pathway_1.html" [])
      , ("l_sections_actions_1", ExecFileRL "z14l/l_sections_actions_1.html" [])
      , ("l_sections_vworld_1", ExecFileRL "z14l/l_sections_vworld_1.html" [])
      , ("l_sections_avatar_1", ExecFileRL "z14l/l_sections_avatar_1.html" [])
      , ("l_sections_vault_1", ExecFileRL "z14l/l_sections_vault_1.html" [])
      , ("l_email_1", ExecFileRL "z14l/l_email_1.html" [])
      , ("l_settings_1", ExecFileRL "z14l/l_settings_1.html" [])
      , ("l_profile_1", ExecFileRL "z14l/l_profile_1.html" [])
      , ("l_help_1", ExecFileRL "z14l/l_help_1.html" [])
      -- Right side menu items:
      , ("r_calendar_1", ExecFileRL "z14l/r_calendar_1.html" [])
      , ("r_notes_1", ExecFileRL "z14l/r_notes_1.html" [])
      , ("r_todos_1", ExecFileRL "z14l/r_todos_1.html" [])
      , ("r_tickets_1", ExecFileRL "z14l/r_tickets_1.html" [])
      , ("r_users_1", ExecFileRL "z14l/r_users_1.html" [])
      , ("r_media_1", ExecFileRL "z14l/r_media_1.html" [])
      , ("r_testrtc_1", ExecFileRL "z14l/r_testrtc_1.html" [])
      , ("r_stats_1", ExecFileRL "z14l/r_stats_1.html" [])
      , ("r_locations_1", ExecFileRL "z14l/r_locations_1.html" [])
      , ("r_new_1", ExecFileRL "z14l/r_new_1.html" [])
      -- For the initial testing:
      , ("stream_1", ExecFileRL "z14l/stream_1.html" [])
      , ("stream_2", ExecFileRL "z14l/stream_2.html" [])
  ]

