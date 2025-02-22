module Wapp.Handlers where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Exception.Safe (tryAny)
import Control.Monad (forever, void)
import qualified Control.Monad.RWS as Rws
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Data.List as L
import qualified Data.Map as Mp
import qualified Data.Set as St
import Prelude as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu
import qualified Network.WebSockets as WS

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Aek
import Data.Aeson (eitherDecode)

import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic (ToServant)

import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H

import Hasql.Pool (Pool)

import qualified HttpSup.Types as Ht
import Wapp.HtmxSupport
import Api.Types (EasyVerseApp, AppEnv (..), Html (..))
import qualified Options.Runtime as Rt

import WordPress.Wrapper (handlePhpRequest)
import Wapp.RouteDef (WappRoutes (..))

import qualified Demo.DemoPage as Dmo
import qualified Wapp.Registry as Wr
import System.FilePath ((</>))

import qualified Wapp.JSSupport as Jss
import Wapp.WsRouter (routeRequest)
import Wapp.Types (RoutingTable, RouteArg (..), ResolvedApp (..)
          , WsSession (..), User (..), UserProfile (..), ExecContext (..)
          , ExecResult (..), Message (..), Status (..), ReferenceEnv (..), AppContext
          , PairReference (..), FunctionReply (..))


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
  rtOpts <- asks config_Ctxt
  let
    targetFile = rtOpts.zb.zbRootPath <> "/mainFrame_1.html"
  content <- liftIO $ Bs.readFile targetFile
  pure . Html . LBS.toStrict $ content

phpTestHdl :: Maybe (Either Text Int) -> EasyVerseApp Html
phpTestHdl mbPostID = do
  _ <- liftIO . P.putStrLn $ "@[indexHdZN] mbPostID: " <> show mbPostID
  rtOpts <- asks config_Ctxt
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
wsStreamInit sid conn = do
  rtOpts <- asks config_Ctxt
  pgDb <- asks pgPool_Ctxt
  routeDict <- asks routeDict_Ctxt
  liftIO $ WS.withPingThread conn 30 (pure ()) $
    case Uu.fromString (T.unpack sid) of
      Nothing -> do
        liftIO . putStrLn $ "@[wsStreamHandler] invalid id: " <> show sid
        pure ()
      Just aid -> do
        -- liftIO $ WS.sendTextData conn ("<div id=\"notifications\" hx-swap-oob=\"beforeend\">Some message</div?" :: Bs.ByteString)
        -- TODO: figure out the routing-table refresh mechanism that doesn't require a websocket reconnection.
        case Mp.lookup aid routeDict of
          Nothing -> do
            liftIO . putStrLn $ "@[wsStreamHandler] no routing table found."
            pure ()
          Just appDef ->
            handleClient rtOpts pgDb appDef
  where
  handleClient :: Rt.RunOptions -> Pool -> ResolvedApp -> IO ()
  handleClient rtOpts pgDb resolvedApp = do
    -- TODO: handle the case with a empty list of libs.
    let
      firstLib = case resolvedApp.libs of
        [] ->
          PairReference { label = "main", ident = "main" }
        (x:_) -> x
    fakeSessionID <- liftIO Uu.nextRandom
    fakeUserID <- liftIO Uu.nextRandom
    (jsSession, jsModule) <- liftIO $ Jss.initJS (T.unpack firstLib.ident) firstLib.label
    let
      refEnv = ReferenceEnv rtOpts pgDb Mp.empty
      session = fakeSession fakeSessionID fakeUserID
      execCtx = ExecContext session resolvedApp Mp.empty RunningST jsSession jsModule
    P.putStrLn $ "@[streamHandler] starting new session: " <> show fakeSessionID
    (result, finalCtxt, messageSet) <- Rws.runRWST socketLoop refEnv execCtx
    Jss.endJS jsSession
    pure ()

  socketLoop :: AppContext ()
  socketLoop = do
    rezA <- tryAny $ forever (streamExec 0)
    case rezA of
      Left err -> do
        liftIO . P.putStrLn $ "@[streamHandler] situation: " <> show err
        liftIO closeConnection
      Right aVal -> do
        liftIO . P.putStrLn $ "@[streamHandler] client disconnected."

  fakeSession :: Uu.UUID -> Uu.UUID -> WsSession
  fakeSession fakeSessionID fakeUserID = WsSession {
    sessionID = fakeSessionID
    , user = User {
      userID = fakeUserID
      , profile = UserProfile {
        name = "fakeUser"
        , email = "fakeUser@example.com"
        , avatar = Nothing
        , prefLocale = "en"
        }
      }
  }

  streamExec :: Int -> AppContext () -- ExecResult, ExecContext, St.Set Message
  streamExec i = do
    refEnv <- Rws.ask
    execCtxt <- Rws.get
    liftIO . P.putStrLn $ "@[streamExec] starting."
    rezA <- liftIO $ WS.receiveDataMessage conn
    case rezA of
      WS.Text msg decodedMsg ->
        let
          hxMsg = eitherDecode msg :: Either String HxWsMessage
        in
        case hxMsg of
          Left err -> do
            liftIO . P.putStrLn $ "@[receiveStream] invalid HxWsMessage: " <> (T.unpack . decodeUtf8 . LBS.toStrict) msg
            liftIO . P.putStrLn $ "@[receiveStream] error: " <> show err
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
                    {-
                    let
                      eiJsonParams = case hxMsg.headers.params of
                        Nothing -> Right $ Ae.Object Aek.empty
                        Just params -> case (Ae.decode $ LBS.fromStrict $ encodeUtf8 params :: Maybe Ae.Value) of
                          Just aValue -> Right aValue
                          _ -> Left $ maybe "@[receiveStream] invalid json params on empty string!" ("@[receiveStream] invalid json params." <>) hxMsg.headers.params
                    -- TODO: need to extract the argMap from the ws-messsage.
                    case eiJsonParams of
                      Left errMsg ->
                        liftIO $ do
                          P.putStrLn $ "@[receiveStream] routeRequest error: " <> show errMsg
                          WS.sendTextData conn ("<div class=\"text-red\"> " <> (Bs.fromStrict . encodeUtf8) errMsg <> "</div>" :: Bs.ByteString)
                      Right jsonParams -> do
                      -}
                        rezA <- liftIO $ routeRequest refEnv execCtxt hxMsg anID jsonParams
                        case rezA of
                          Left errMsg ->
                            liftIO $ do
                              P.putStrLn $ "@[receiveStream] routeRequest error: " <> show errMsg
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
        liftIO . P.putStrLn $ "@[receiveStream] received binary."
    pure ()

  closeConnection = do
    WS.sendClose conn ("Bye" :: Bs.ByteString)
    void $ WS.receiveDataMessage conn


xStaticHdl :: [String] -> EasyVerseApp Html
xStaticHdl segments = do
  rtOpts <- asks config_Ctxt
  let
    -- Used to have the "/xstatic/" path in between root & segments.
    targetFile = rtOpts.zb.zbRootPath </> L.intercalate "/" segments
  liftIO . putStrLn $ "@[xStaticHandler] pageUrl: " <> targetFile
  pageContent <- liftIO $ LBS.readFile targetFile
  pure . Html . LBS.toStrict $ pageContent
