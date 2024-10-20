module Commands.Server where

import Network.Wai.Handler.Warp as Wrp

import Control.Exception (bracket)
import Control.Monad.Cont (runContT, ContT (..))    -- liftIO

import System.Posix (installHandler)


import Database.MySQL.Base (connect, defaultConnectInfo, query_, ConnectInfo(..))


import ServeApi (serveApi)
import Api.Handlers (setupWai)
import WordPress.Wrapper (defineSapiModuleStruct, endPhp)
import DB.Connect (startMql, startPg)
import Options.Runtime as Rto


serverCmd :: Rto.RunOptions -> IO ()
serverCmd rtOpts = do
  putStrLn $ "@[serverHu] starting, opts: " <> show rtOpts
  putStrLn "tests done."
  sapiModuleDef <- defineSapiModuleStruct
  let
    mqlConn = startMql rtOpts.wp.mqlDbConf
    pgPool = startPg rtOpts.pgDbConf
    contArgs = (,,) <$> pgPool <*> mqlConn <*> pure sapiModuleDef
  runContT contArgs mainAction
  where
  mainAction (pgPool, mqlConn, sapiModuleDef) = do
    let 
      settings = setupWai rtOpts.serverPort rtOpts.serverHost (globalShutdownHandler sapiModuleDef)
    webHandler <- serveApi rtOpts pgPool mqlConn sapiModuleDef
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

