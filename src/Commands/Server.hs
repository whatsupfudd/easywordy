module Commands.Server where

import Network.Wai.Handler.Warp as Wrp

import Control.Exception (bracket)
import Control.Monad.Cont (runContT, ContT (..))    -- liftIO

import System.Posix (installHandler)


import Database.MySQL.Base (connect, defaultConnectInfo, query_, ConnectInfo(..))


import ServeApi (serveApi)
import Api.Handlers (setupWai)
import WordPress.Wrapper (invokeFile)
import DB.Connect (startMql, startPg)
import Options.Runtime as Rto


serverCmd :: Rto.RunOptions -> IO ()
serverCmd rtOpts = do
  putStrLn $ "@[serverHu] starting, opts: " <> show rtOpts
  putStrLn "tests done."
  let
    mqlConn = startMql rtOpts.wp.mqlDbConf
    pgPool = startPg rtOpts.pgDbConf
    contArgs = (,) <$> pgPool <*> mqlConn
  runContT contArgs mainAction     --  (start rtOpts.db)
  where
  mainAction (pgPool, mqlConn) = do
    let 
      settings = setupWai rtOpts.serverPort rtOpts.serverHost globalShutdownHandler
    webHandler <- serveApi rtOpts pgPool mqlConn
    Wrp.runSettings settings webHandler
    putStrLn $ "@[serverHu] ending."
    pure ()
  globalShutdownHandler =
    putStrLn "@[globalShutdownHandler] done."


mainBgTask :: ContT r IO ()
mainBgTask =
  let
    defaultStart = putStrLn "@[mainBgTask] Starting default bg thread."
    defaultFinish _ = putStrLn "@[mainBgTask] finishing default bg thread."
  in do
  ContT $ bracket defaultStart defaultFinish

