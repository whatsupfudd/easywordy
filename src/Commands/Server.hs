module Commands.Server where

import qualified Control.Concurrent.STM as Cs
import Control.Exception (bracket)
import Control.Monad.Cont (runContT, ContT (..))    -- liftIO

import System.Posix (installHandler)

import Database.MySQL.Base (connect, defaultConnectInfo, query_, ConnectInfo(..))

import Network.Wai.Handler.Warp as Wrp

import Api.Handlers (setupWai)
import Wapp.Internal.WordPress.Wrapper (defineSapiModuleStruct, endPhp)
import DB.Connect (startMql, startPg)
import ServeApi (launchServant)
import Wapp.FileWatcher (newWatcher)
import Wapp.Registry (loadAppDefs)
import Options.Runtime as Rto

serverCmd :: Rto.RunOptions -> IO ()
serverCmd rtOpts = do
  putStrLn $ "@[serverCmd] starting, wapp: " <> show rtOpts.wapp <> "\n  zhopness: " <> show rtOpts.zb
  sapiModuleDef <- defineSapiModuleStruct
  eiRouteDict <- loadAppDefs rtOpts.wapp
  -- TODO: make the creation of defWatcher conditional to the runtime config.
  newCommChannel <- Cs.newTVarIO ""
  newUpdateSignal <- Cs.newEmptyTMVarIO
  defWatcher <- newWatcher rtOpts.wapp.waDefDir newCommChannel newUpdateSignal
  case eiRouteDict of
    Left errMsg -> do
      putStrLn $ "@[serverCmd] error loading app defs: " <> errMsg
      return ()
    Right routeDict ->
      let
        mqlConn = startMql rtOpts.wp.mqlDbConf
        pgPool = startPg rtOpts.pgDbConf
        contArgs = (,,,) <$> pgPool <*> mqlConn <*> pure sapiModuleDef <*> pure (routeDict, Just defWatcher)
      in do
        putStrLn "@[serverCmd] going to start mainAction."
        runContT contArgs mainAction
  where
  mainAction (pgPool, mqlConn, sapiModuleDef, appDefs) = do
    putStrLn $ "@[mainAction] starting."
    let
      settings = setupWai rtOpts.serverPort rtOpts.serverHost (globalShutdownHandler sapiModuleDef)
    webHandler <- launchServant rtOpts pgPool mqlConn sapiModuleDef appDefs
    putStrLn $ "@[mainAction] webHandler started."
    Wrp.runSettings settings webHandler
    putStrLn $ "@[serverCmd] ending."
    pure ()
  globalShutdownHandler sapiModuleDef = do
    endPhp sapiModuleDef
    putStrLn "@[globalShutdownHandler] done."


mainBgTask :: ContT r IO ()
mainBgTask =
  let
    defaultStart = putStrLn "@[mainBgTask] Starting default bg thread."
    defaultFinish _ = putStrLn "@[mainBgTask] finishing default bg thread."
  in do
  ContT $ bracket defaultStart defaultFinish

