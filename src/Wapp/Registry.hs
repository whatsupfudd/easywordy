module Wapp.Registry where

import Control.Monad (forM)

import Data.Char (isLetter)
import Data.Either (lefts, rights)
import Data.List (intercalate)
import qualified Data.Map as Mp
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import qualified System.Directory as Sd
import qualified System.Environment as Se
import qualified System.FilePath.Posix as Spx
import qualified System.IO.Error as Er
import System.FilePath.Posix ((</>))

import qualified Data.Yaml as Y

import qualified Options.Runtime as Rt
import Wapp.InternalLib (buildInternalLibrary, LibraryMap)
import qualified DB.Connect as Db
import qualified Wapp.Types as Wt
import qualified Wapp.AppDef as Wd


loadAppDefs :: Rt.WappConfig -> IO (Either String Wd.RoutingDictionary)
loadAppDefs wappConf = do
  let
    fctResolver = defaultFctResolver buildInternalLibrary
  appDefs <- Sd.listDirectory wappConf.waDefDir
  let
    validFiles = filter (\filePath -> Spx.takeExtension filePath == ".yaml" && isLetter (head filePath)) appDefs
  rezA <- forM validFiles $ \filePath ->
    let
      appDefFile = Spx.joinPath [wappConf.waDefDir, filePath]
    in do
    rezC <- Y.decodeFileEither appDefFile :: IO (Either Y.ParseException Wd.AppDef)
    case rezC of
      Left err ->
        pure . Left $ (filePath, err)
      Right appDef ->
        pure . Right $ appDef
  case lefts rezA of
    [] -> do
      rezB <- forM (rights rezA) $ \appDef ->
        resolveAppDef wappConf fctResolver appDef
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


defaultFctResolver :: LibraryMap -> Wd.FctResolver
defaultFctResolver localLibs = Wd.FctResolver {
  resolveLib = Right
  , resolveFct = functionResolver localLibs
  }


functionResolver :: LibraryMap -> Mp.Map Text Text -> Wd.FunctionDef -> Either String (Text, Wd.RouteLogic)
functionResolver localLibs libs fctDef =
  case fctDef.action of
    Wd.FileVerbatimAD filePath ->
      Right (fctDef.fid, Wd.ExecFileRL filePath [])
    Wd.TemplateAD filePath ->
      Left "Not implemented"
    Wd.FunctionAD pairRef ->
      case Mp.lookup pairRef.label localLibs of
        Just aLib ->
          case Mp.lookup pairRef.ident aLib of
            Just aFct ->
              Right (fctDef.fid, Wd.FunctionRL $ Wd.Internal aFct)
            Nothing ->
              Left $ "@[functionResolver] unknown internal library function: " <> unpack pairRef.ident
        Nothing ->
          case Mp.lookup pairRef.label libs of
            Just aPath ->
              Right (fctDef.fid, Wd.FunctionRL $ Wd.External (aPath, pairRef.label, pairRef.ident))
            Nothing ->
              Left $ "@[functionResolver] unknown external library function: " <> unpack pairRef.label <> ":" <> unpack pairRef.ident


resolveAppDef :: Rt.WappConfig -> Wd.FctResolver -> Wd.AppDef -> IO (Either String Wd.ResolvedApp)
resolveAppDef wappConf fctResolver appDef =
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
          let
            mbDbConf = case appDef.db of
              Just dbConf ->
                Just $ Db.defaultPgDbConf {
                  Db.host = encodeUtf8 dbConf.host
                  , Db.user = encodeUtf8 dbConf.user
                  , Db.passwd = encodeUtf8 dbConf.password
                  , Db.dbase = encodeUtf8 dbConf.dbname
                  }
              Nothing -> Nothing
          in
          Right $ Wd.ResolvedApp {
              aid = appDef.uid
              , label = appDef.label
              , locales = appDef.locales
              , rootPath = wappConf.waContentDir </> appDef.rootPath
              , libs = libRefs
              , functions = Mp.fromList (rights eiFcts)
              , db = mbDbConf
            }
        fctErrs ->
          Left $ "Error resolving functions: " <> intercalate "\n" fctErrs
    libErrs ->
      Left $ "Error resolving libs: " <> intercalate "\n" libErrs
