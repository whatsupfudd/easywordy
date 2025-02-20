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

import Options.Runtime (RunOptions (..), ZbConfig (..))

import Control.Lens.Internal.CTypes (Int32)
import Data.Word (Word8)

import Wapp.HtmxSupport (HxWsMessage (..))
import Wapp.JSSupport as Jss (runElmFunction, JSReturn (..))
import Wapp.Types

routeRequest :: ReferenceEnv -> ExecContext -> HxWsMessage -> Text -> Ae.Value -> IO (Either String Lbs.ByteString)
routeRequest refEnv execCtxt hxMsg anID jsonParams =
  case Mp.lookup anID execCtxt.resolvedApp.functions of
    Nothing -> do
      putStrLn $ "@[routeRequest] templatePath not found: " <> show anID
      pure . Left $ "ERROR: templatePath not found: " <> T.unpack anID
    Just (ExecFileRL templatePath _) -> do
      putStrLn $ "@[routeRequest] templatePath: " <> templatePath
      response <- liftIO $ Lbs.readFile (refEnv.runOpts.zb.zbRootPath </> templatePath)
      -- putStrLn $ "@[receiveStream] sending " <> show (Bs.length response) <> " bytes."
      pure . Right $ response
    Just (FunctionRL fetchFunc) -> do
      putStrLn $ "@[routeRequest] function: " <> T.unpack anID
      -- The 'fmap fromStrict' creates the monadic converter, and the '<$>' applies it
      -- into the IO instance.
      case fetchFunc of
        Internal fct ->
          fmap Lbs.fromStrict <$> fct refEnv.runOpts refEnv.pgPool (jsonParams, hxMsg.content)
        External (libPath, moduleName,fctName) -> do
          rezA <- try $ Jss.runElmFunction execCtxt.jsSession execCtxt.jsModule moduleName fctName jsonParams
          case rezA of
            Left err -> do
              putStrLn $ "@[routeRequest] Jss.runElmFunction err: " <> show (err :: SomeException)
              pure . Left $ "ERROR: Jss.runElmFunction err: " <> show (err :: SomeException)
            Right jsReturn -> do
              putStrLn "@[routeRequest] Jss.runElmFunction finished."
              case jsReturn.result of
                "ok" ->
                  pure . Right $ Lbs.fromStrict . T.encodeUtf8 $ jsReturn.content
                "err" ->
                  pure . Left $ "ERROR: " <> T.unpack jsReturn.content
