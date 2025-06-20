{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Wapp.Handlers where

import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.RWS.Lazy as Rws
-- Gm => Global Monad
import qualified Control.Monad.Reader as Gm
import Control.Exception.Safe (tryAny, bracket)
import Control.Exception (throw, SomeException (..))
import Control.Monad.Catch (throwM)
import Control.Monad (forever, void, when)
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import qualified Control.Concurrent.STM as Cs

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Maybe as Mb
import qualified Data.Set as St
import Prelude as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu
import qualified Network.WebSockets as WS

import Data.Int (Int32)
import qualified Data.Vector as Vc

import System.FSNotify
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Aek
import Data.Aeson (eitherDecode)

import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic (ToServant)

import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H

import qualified Hasql.Connection as DbC
import qualified Hasql.Session as DbS
import qualified Hasql.Statement as DbS
import qualified Hasql.Encoders as DbE
import qualified Hasql.Decoders as DbD
import Hasql.Pool (Pool, acquire, use)

import qualified HttpSup.Types as Ht
import Api.Types (EasyVerseApp (..), AppEnv (..), Html (..))
import qualified Options.Runtime as Rt
import qualified DB.Connect as Db

import WordPress.Wrapper (handlePhpRequest)
import Wapp.HtmxSupport
import Wapp.RouteDef (WappRoutes (..))

import qualified Demo.DemoPage as Dmo
import qualified Wapp.Registry as Wr

import qualified Wapp.JSSupport as Jss
import Wapp.WsRouter (routeRequest)
import Wapp.FileWatcher (WatcherControl (..), newWatcher)
import Wapp.Types (
          ClientContext (..), ExecResult (..), Message (..)
          , Status (..), ReferenceEnv (..), AppContext
          , JSExecSupport (..), AppEvent (..), ClientMessage (..))
import Wapp.AppDef (RoutingTable, RouteArg (..), ResolvedApp (..)
          , PairReference (..), FunctionReply (..))
import Wapp.State (User (..), UserProfile (..), fakeSession, WappState (..), Session (..), LiveWapp (..))


wappHandlers :: ToServant WappRoutes (AsServerT EasyVerseApp)
wappHandlers =
  genericServerT $ WappRoutes {
    phpTest = phpTestHdl
    , xStatic = xStaticHdl
    , wsStream = wsStreamInit
    , rootZN = welcomeHdZN
  }

welcomeHdZN :: EasyVerseApp Html
welcomeHdZN = do
  rtOpts <- Gm.asks config_Ctxt
  let
    targetFile = rtOpts.zb.zbRootPath <> "/mainFrame_1.html"
  content <- liftIO $ Bs.readFile targetFile
  pure . Html . Lbs.toStrict $ content

phpTestHdl :: Maybe (Either Text Int) -> EasyVerseApp Html
phpTestHdl mbPostID = do
  _ <- liftIO . P.putStrLn $ "@[indexHdZN] mbPostID: " <> show mbPostID
  rtOpts <- Gm.asks config_Ctxt
  let
    targetUrl = "zpns/index.php"
    argMap = case mbPostID of
      Nothing -> Mp.empty :: Mp.Map String String
      Just (Left errMsg) -> Mp.singleton "err" (T.unpack errMsg)
      Just (Right postID) -> Mp.singleton "p" (show postID)
  (aStr, duration) <-
        liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
              , uri = targetUrl, queryArgs = argMap
              , reqHeaders = [], reqBody = Nothing })
  pure . Html $ aStr


-- MonadIO m => WS.Connection -> m ()
wsStreamInit :: Text -> WS.Connection -> EasyVerseApp ()
wsStreamInit appID conn = do
  rtOpts <- Gm.asks config_Ctxt
  pgDb <- Gm.asks pgPool_Ctxt
  eiTargetApp <- getTargetApp (T.unpack appID)

  case eiTargetApp of
    Left errMsg -> do
      liftIO . putStrLn $ "@[wsStreamInit] error: " <> show errMsg
      pure ()
    Right aLiveApp -> do
      liftIO $ WS.withPingThread conn 30 (pure ()) $ do
        -- liftIO $ WS.sendTextData conn ("<div id=\"notifications\" hx-swap-oob=\"beforeend\">Some message</div?" :: Bs.ByteString)
        -- TODO: figure out the routing-table refresh mechanism that doesn't require a websocket reconnection.
        fakeSessionID <- liftIO Uu.nextRandom
        fakeUserID <- liftIO Uu.nextRandom
        let
          firstLib = case aLiveApp.wapp.libs of
            [] ->
              PairReference { label = "main", ident = "main" }
            (x:_) -> x
        jsSupport <- case firstLib.ident of
          "" -> pure Nothing
          anIdent -> do
            (jsSession, jsModule) <- do
              liftIO $ Jss.initJS (aLiveApp.wapp.rootPath </> T.unpack anIdent) firstLib.label
            pure $ Just $ JSExecSupport jsSession jsModule
        let
          newContext = ClientContext {
            liveApp = aLiveApp
            , heap = Mp.empty
            , status = RunningST
            , jsSupport = jsSupport
            , session = fakeSession fakeSessionID fakeUserID
            , actionList = []
            }
        handleClient rtOpts pgDb newContext
  where
  getTargetApp :: String -> EasyVerseApp (Either String LiveWapp)
  getTargetApp reqID = do
    wState <- Gm.asks state_Tmp
    case Uu.fromString reqID of
      Nothing -> do
        liftIO . putStrLn $ "@[getTargetApp] invalid id: " <> reqID
        pure $ Left $ "@[getTargetApp] invalid id: " <> reqID
      Just aUID -> do
        liftIO $ P.putStrLn $ "@[getTargetApp] looking for appID: " <> show aUID
        liftIO $ P.putStrLn $ "@[getTargetApp] cache: " <> show wState.cache
        case Mp.lookup aUID wState.cache of
          Nothing -> do
            case Mp.lookup aUID wState.appDefs of
              Nothing ->
                pure . Left $ "@[getTargetApp] no routing table found, appID: " <> show aUID
              Just appDef -> do
                -- TODO: make the creation of newWatcher conditional to the appDef config.
                -- TODO: create signaler/commChannel here, pass to newWatcher instead of letting it create.
                newCommChannel <- liftIO $ Cs.newTVarIO ""
                newUpdateSignal <- liftIO $ Cs.newEmptyTMVarIO
                newWatcher <- liftIO $ newWatcher appDef.rootPath newCommChannel newUpdateSignal
                mbDb <- case appDef.db of
                  Just dbConf ->
                    let
                      dbSettings = DbC.settings dbConf.host dbConf.port dbConf.user dbConf.passwd dbConf.dbase
                    in do
                    liftIO . putStrLn $ "@[wsStreamInit] acquiring db pool, " <> (T.unpack . decodeUtf8) dbConf.host <> ", user: " <>(T.unpack . decodeUtf8) dbConf.user <> ", db:" <> (T.unpack . decodeUtf8) dbConf.dbase
                    Just <$> liftIO (acquire dbConf.poolSize dbConf.acqTimeout dbConf.poolTimeOut dbSettings)
                  Nothing -> pure Nothing
                let
                  newApp = LiveWapp {
                    wapp = appDef
                    , watcher = Just newWatcher
                    , signaler = newUpdateSignal
                    , commChannel = newCommChannel
                    , db = mbDb
                    }
                let
                  newState = wState { cache = Mp.insert aUID newApp wState.cache }
                liftIO $ P.putStrLn $ "@[getTargetApp] adding new liveApp, ID: " <> show aUID
                pure $ Right newApp
          Just liveApp -> do
            liftIO $ P.putStrLn $ "@[getTargetApp] found liveApp, ID: " <> show aUID
            pure $ Right liveApp

  handleClient :: Rt.RunOptions -> Pool -> ClientContext -> IO ()
  handleClient rtOpts pgDb execCtxt = do
    -- TODO: handle the case with a empty list of libs.
    let
      dbPool = Mb.fromMaybe pgDb execCtxt.liveApp.db
      refEnv = ReferenceEnv rtOpts dbPool Mp.empty execCtxt.liveApp.signaler execCtxt.liveApp.commChannel
    putStrLn $ "@[handleClient] starting new session: " <> show execCtxt.session.idSE
    (result, finalCtxt, messageSet) <- Rws.runRWST startClientSession refEnv execCtxt
    putStrLn $ "@[handleClient] ending session: " <> show execCtxt.session.idSE
    case execCtxt.jsSupport of
      Nothing -> pure ()
      Just jsSupport -> Jss.endJS jsSupport.jsSession
    case execCtxt.liveApp.watcher of
      Nothing -> pure ()
      Just watcherControl -> do
        putStrLn "@[handleClient] stopping watcher."
        watcherControl.stopWatcher
    pure ()


  startClientSession :: AppContext ()
  startClientSession = do
    rezA <- tryAny $ forever (clientIteration 0)
    case rezA of
      Left err -> do
        liftIO . P.putStrLn $ "@[startClientSession] situation: " <> show err
        rezA <- liftIO closeConnection
        pure ()
      Right aVal -> do
        liftIO . P.putStrLn $ "@[startClientSession] client disconnected."


  clientIteration :: Int -> AppContext () -- ExecResult, ClientContext, St.Set Message
  clientIteration i = do
    refEnv <- Rws.ask
    execCtxt <- Rws.get
    -- liftIO . P.putStrLn $ "@[clientIteration] starting."
    eiRezA <- liftIO $ interruptibleReceive conn refEnv.srvUpdate refEnv.commChannel
    --liftIO . P.putStrLn $ "@[clientIteration] received message."
    case eiRezA of
      Left errMsg -> do
        liftIO . putStrLn $ "@[clientIteration] error: " <> errMsg
        throwM $ userError errMsg
      Right clientMsg -> do
        -- liftIO . P.putStrLn $ "@[clientIteration] processing message: " <> show clientMsg
        case clientMsg of
          ErrorCM errMsg -> do
            liftIO . putStrLn $ "@[clientIteration] error: " <> errMsg
          EventCM anEvent ->
            case anEvent of
              FileUpdateAE aPath -> do
                -- liftIO . putStrLn $ "@[clientIteration] got file update: " <> aPath
                let
                  repeatMsg = "{\"msgType\":\"msg\", \"message\":\"file update: " <> (Bs.fromStrict . encodeUtf8 . T.pack) aPath <> "\"}"
                  firstLib = case execCtxt.liveApp.wapp.libs of
                    [] ->
                      PairReference { label = "main", ident = "main" }
                    (x:_) -> x
                case firstLib.ident of
                  "" -> pure ()
                  anIdent -> do
                    when (execCtxt.liveApp.wapp.rootPath </> T.unpack anIdent == aPath) $ do
                      (!jsSession, !jsModule) <- liftIO $ Jss.initJS aPath firstLib.label
                      let
                        newContext = execCtxt { jsSupport = Just $ JSExecSupport jsSession jsModule }
                      Rws.put newContext
                      case execCtxt.actionList of
                        [] -> pure ()
                        (hxMsg, anID, jsonParams) : _ -> do
                          liftIO . putStrLn $ "@[clientIteration] replay last action."
                          rezA <- liftIO $ routeRequest refEnv newContext hxMsg anID jsonParams
                          case rezA of
                            Left errMsg ->
                              liftIO $ do
                                putStrLn $ "@[clientIteration] routeRequest error: " <> show errMsg
                                WS.sendTextData conn ("<div class=\"text-red\"> " <> (Bs.fromStrict . encodeUtf8 . T.pack) errMsg <> "</div>" :: Bs.ByteString)
                            Right aReply ->
                              let
                                target = case hxMsg.headers.target of
                                  Just aValue -> "id=\"" <> (Bs.fromStrict . encodeUtf8) aValue <> "\""
                                  Nothing -> ""
                                (modifiers, body) = case aReply of
                                  BasicFR aHtml -> ("", aHtml)
                                  AppendChildFR aHtml -> ("hx-swap-oob=\"beforeend\"", aHtml)
                                  AfterEndFR aHtml -> ("hx-swap-oob=\"afterend\"", aHtml)
                                response = "<div" <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s) <> ">" <> body <> "</div>"
                              in
                              liftIO $ WS.sendTextData conn response
              OutOfBandReplyAE aMsg -> do
                liftIO . P.putStrLn $ "@[clientIteration] out-of-band reply."
                liftIO $ WS.sendTextData conn  ( aMsg :: Bs.ByteString)
              IdlingAE ->
                pure ()
          MessageCM aMsg -> do
            processIncomingMessage conn aMsg


  processIncomingMessage :: WS.Connection -> WS.DataMessage -> AppContext ()
  processIncomingMessage conn aMsg = do
    refEnv <- Rws.ask
    execCtxt <- Rws.get
    liftIO $ putStrLn $ "@[processIncomingMessage] aMsg: " <> show aMsg
    case aMsg of
      WS.Text msg decodedMsg ->
        let
          hxMsg = eitherDecode msg :: Either String HxWsMessage
        in
        case hxMsg of
          Left err -> do
            liftIO . P.putStrLn $ "@[clientIteration] invalid HxWsMessage: " <> (T.unpack . decodeUtf8 . Lbs.toStrict) msg
            liftIO . P.putStrLn $ "@[clientIteration] error: " <> show err
          Right hxMsg ->
            case hxMsg.wsMessage of
              Nothing ->
                case hxMsg.headers.mid of
                  Nothing ->
                    liftIO $ WS.sendTextData conn  ("<div class=\"text-red\"> NO MID</div>" :: Bs.ByteString)
                  Just anID ->
                    let
                      jsonParams = case hxMsg.headers.params of
                        Nothing -> Ae.Object Aek.empty
                        Just params -> params
                    in do
                      let
                        newAction = (hxMsg, anID, jsonParams)
                      Rws.modify $ \execCtxt -> execCtxt { actionList = newAction : execCtxt.actionList }
                      rezA <- liftIO $ routeRequest refEnv execCtxt hxMsg anID jsonParams
                      case rezA of
                        Left errMsg ->
                          liftIO $ do
                            P.putStrLn $ "@[clientIteration] routeRequest error: " <> show errMsg
                            WS.sendTextData conn ("<div class=\"text-red\"> " <> (Bs.fromStrict . encodeUtf8 . T.pack) errMsg <> "</div>" :: Bs.ByteString)
                        Right aReply ->
                          let
                            target = case hxMsg.headers.target of
                              Just aValue -> "id=\"" <> (Bs.fromStrict . encodeUtf8) aValue <> "\""
                              Nothing -> ""
                            (modifiers, body) = case aReply of
                              BasicFR aHtml -> ("", aHtml)
                              AppendChildFR aHtml -> ("hx-swap-oob=\"beforeend\"", aHtml)
                              AfterEndFR aHtml -> ("hx-swap-oob=\"afterend\"", aHtml)
                            response = "<div" <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s) <> ">" <> body <> "</div>"
                          in
                          liftIO $ WS.sendTextData conn response
              Just aText ->
                liftIO $ WS.sendTextData conn $ H.renderHtml $ Dmo.demoReply hxMsg.wsMessage
      WS.Binary msg ->
        liftIO . P.putStrLn $ "@[clientIteration] received binary."


  interruptibleReceive :: WS.Connection -> Cs.TMVar () -> Cs.TVar FilePath -> IO (Either String ClientMessage)
  interruptibleReceive conn updateSignal commChannel = do
    -- Fork a thread to receive the next message
    resultVar <- Cs.newEmptyTMVarIO
    receiverThread <- forkIO $ do
      result <- tryAny $ WS.receiveDataMessage conn
      case result of
        Right msg -> Cs.atomically $ Cs.putTMVar resultVar (Right $ MessageCM msg)
        Left err -> Cs.atomically $ Cs.putTMVar resultVar (Left $ show err)

    -- Wait for either a message or an update signal
    result <- Cs.atomically $ (do
        -- Check for an external update
        () <- Cs.takeTMVar updateSignal
        -- Put the signal back for other handlers
        -- Ct.putTMVar updateSignal ()
        filePath <- Cs.readTVar commChannel
        pure . Right . EventCM $ FileUpdateAE filePath
      ) `Cs.orElse` (do
        -- Check for incoming message
        Cs.takeTMVar resultVar
      )

    -- Clean up the receiver thread if it's still running
    killThread receiverThread
    return result


  closeConnection = do
    WS.sendClose conn ("Bye" :: Bs.ByteString)
    tryAny $ WS.receiveDataMessage conn




xStaticHdl :: [String] -> EasyVerseApp Html
xStaticHdl segments = do
  rtOpts <- Rws.asks config_Ctxt
  let
    -- Used to have the "/xstatic/" path in between root & segments.
    targetFile = rtOpts.zb.zbRootPath </> L.intercalate "/" segments
  liftIO . putStrLn $ "@[xStaticHandler] pageUrl: " <> targetFile
  pageContent <- liftIO $ Lbs.readFile targetFile
  pure . Html . Lbs.toStrict $ pageContent
