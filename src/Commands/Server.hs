module Commands.Server where

import Network.Wai.Handler.Warp as Wrp

import Control.Exception (bracket)
import Control.Monad.Cont (runContT, ContT (..))    -- liftIO

import System.Posix (installHandler)


import Database.MySQL.Base (connect, defaultConnectInfo, query_, ConnectInfo(..))


import Api.Handlers (setupWai)
import WordPress.Wrapper (defineSapiModuleStruct, endPhp)
import DB.Connect (startMql, startPg)
import ServeApi (launchServant)
import Wapp.Registry (loadAppDefs)
import Options.Runtime as Rto

serverCmd :: Rto.RunOptions -> IO ()
serverCmd rtOpts = do
  putStrLn $ "@[serverHu] starting, opts: " <> show rtOpts
  putStrLn "tests done."
  sapiModuleDef <- defineSapiModuleStruct
  eiRouteDict <- loadAppDefs rtOpts.appDefs
  case eiRouteDict of
    Left errMsg -> do
      putStrLn $ "@[serverCmd] error loading app defs: " <> errMsg
      return ()
    Right routeDict ->
      let
        mqlConn = startMql rtOpts.wp.mqlDbConf
        pgPool = startPg rtOpts.pgDbConf
        contArgs = (,,,) <$> pgPool <*> mqlConn <*> pure sapiModuleDef <*> pure routeDict
      in do
      runContT contArgs mainAction
  where
  mainAction (pgPool, mqlConn, sapiModuleDef, routeDict) = do
    let 
      settings = setupWai rtOpts.serverPort rtOpts.serverHost (globalShutdownHandler sapiModuleDef)
    webHandler <- launchServant rtOpts pgPool mqlConn sapiModuleDef routeDict
    Wrp.runSettings settings webHandler
    putStrLn $ "@[serverHu] ending."
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

