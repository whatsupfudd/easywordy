module Wapp.WsRouter where

import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath.Posix ((</>))

import qualified Data.Aeson as Ae

import Hasql.Pool (Pool)

import Options.Runtime (RunOptions (..), WappConfig (..))

import Control.Lens.Internal.CTypes (Int32)
import Data.Word (Word8)

import Wapp.HtmxSupport (HxWsMessage (..))
import Wapp.JSSupport as Jss (runElmFunction, JSReturn (..))
import qualified Wapp.AppDef as Wd
import Wapp.State (LiveWapp (..))
import Wapp.Types

routeRequest :: ReferenceEnv -> ClientContext -> HxWsMessage -> Text -> Ae.Value -> IO (Either String Wd.FunctionReply)
routeRequest refEnv execCtxt hxMsg anID jsonParams =
  case Mp.lookup anID execCtxt.liveApp.wapp.functions of
    Nothing -> do
      putStrLn $ "@[routeRequest] templatePath not found: " <> show anID
      -- putStrLn $ "@[routeRequest] fct map: " <> show execCtxt.liveApp.wapp.functions
      pure . Left $ "ERROR: templatePath not found: " <> T.unpack anID
    Just (Wd.ExecFileRL templatePath _) -> do
      putStrLn $ "@[routeRequest] templatePath: " <> templatePath
      rezA <- try $
        liftIO $ Lbs.readFile (refEnv.runOpts.wapp.waContentDir </> execCtxt.liveApp.wapp.rootPath </> templatePath)
      case rezA of
        Left err -> do
          putStrLn $ "@[routeRequest] error reading file: " <> show (err :: SomeException)
          pure . Left $ "ERROR: error reading file: " <> show (err :: SomeException)
        Right response -> do
          -- putStrLn $ "@[routeRequest] sending " <> show (Lbs.length response) <> " bytes."
          pure . Right $ Wd.BasicFR (response, Nothing)
    Just (Wd.FunctionRL fetchFunc) -> do
      putStrLn $ "@[routeRequest] function: " <> T.unpack anID
      -- The 'fmap fromStrict' creates the monadic converter, and the '<$>' applies it
      -- into the IO instance.
      case fetchFunc of
        Wd.Internal fct ->
          fct refEnv.runOpts refEnv.pgPool (jsonParams, hxMsg.content)
        Wd.External (libPath, moduleName,fctName) ->
          case execCtxt.jsSupport of
            Nothing -> do
              putStrLn "@[routeRequest] no jsSupport."
              pure . Left $ "ERROR: no jsSupport."
            Just jsSupport -> do
              rezA <- try $ Jss.runElmFunction jsSupport execCtxt.liveApp.db moduleName fctName jsonParams
              case rezA of
                Left err -> do
                  putStrLn $ "@[routeRequest] Jss.runElmFunction err: " <> show (err :: SomeException)
                  pure . Left $ "ERROR: Jss.runElmFunction err: " <> show (err :: SomeException)
                Right jsReturn -> do
                  putStrLn "@[routeRequest] Jss.runElmFunction finished."
                  case jsReturn.result of
                    "ok" ->
                      pure . Right $ Wd.BasicFR (Lbs.fromStrict . T.encodeUtf8 $ jsReturn.content, jsReturn.container)
                    "err" ->
                      pure . Left $ "ERROR: " <> T.unpack jsReturn.content
