module Commands.Server where

import Network.Wai.Handler.Warp as Wrp
import Control.Monad.Cont (runContT)

import Control.Exception (bracket)
import Control.Monad.Cont (ContT (..))    -- liftIO

-- import DB.Connect (start)
import Api (serveApi)
import Api.Handlers (setupWai)
import Options.Runtime as Rto

serverCmd :: Rto.RunOptions -> IO ()
serverCmd rtOpts = do
  putStrLn $ "@[serverHu] starting, opts: " <> show rtOpts
  runContT (fakeBgTask) mainAction     --  (start rtOpts.db)
  where
  mainAction _ = do
    let shutdownHandler = putStrLn "@[Servant.run] Terminating..."
        settings = setupWai rtOpts.serverPort rtOpts.serverHost shutdownHandler
    webHandler <- serveApi rtOpts -- dbPool
    Wrp.runSettings settings webHandler
    putStrLn $ "@[serverHu] ending."
    pure ()


fakeBgTask :: ContT r IO ()
fakeBgTask =
  let
    fakeStart = putStrLn "starting fake bg thread."
    fakeFinish _ = putStrLn "finishing fake bg thread."
  in do
  ContT $ bracket fakeStart fakeFinish
