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
import Control.Monad (forever, void, when, forM_)
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
import System.Directory (renameFile, copyFile, copyFileWithMetadata, removeFile)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Aek
import Data.Aeson (eitherDecode)

import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic (ToServant)
import Servant.Multipart (MultipartData (..), Tmp, FileData (..), Mem)
import Servant.API (NoContent (..), JSON, Put, Patch, Post, ReqBody, DeleteNoContent)
import Servant.API.QueryParam (QueryParam)
import Servant.API.Modifiers (Optional, Lenient, Required, Strict)
import Servant (throwError, err303, errBody, errHeaders)


import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H

import qualified Hasql.Connection as DbC
import qualified Hasql.Session as DbS
import qualified Hasql.Statement as DbS
import qualified Hasql.Encoders as DbE
import qualified Hasql.Decoders as DbD
import Hasql.Pool (Pool, acquire, use)

import qualified HttpSup.Types as Ht
import Api.Types (EasyVerseApp (..), AppEnv (..), Html (..), ApiError (RedirectAE))
import qualified Options.Runtime as Rt
import qualified DB.Connect as Db

import Wapp.Internal.WordPress.Wrapper (handlePhpRequest)
import Wapp.HtmxSupport
import Wapp.RouteDef (WappRoutes (..), WappAuthRoutes (..), WappUserRoutes (..), WappAuthzRoutes (..), WappAdminRoutes (..)
  , WappAdminUsersRoutes (..), WappAdminWappsRoutes (..), WappAdminWappMembersRoutes (..)
  , WappUIRoutes (..), WappUIAuthRoutes (..)
  )

import qualified Demo.DemoPage as Dmo
import qualified Wapp.Registry as Wr

import qualified Wapp.JSSupport as Jss
import qualified Wapp.InternalLib as ILib
import Wapp.WsRouter (routeRequest)
import Wapp.FileWatcher (WatcherControl (..), newWatcher)
import Wapp.Types (
          ClientContext (..), ExecResult (..), Message (..)
          , Status (..), ReferenceEnv (..), AppContext
          , JSExecSupport (..), AppEvent (..), ClientMessage (..))
import Wapp.AppDef (RoutingTable, RouteArg (..), ResolvedApp (..)
          , PairReference (..), FunctionReply (..), RequestParams (..))
import Wapp.State (User (..), UserProfile (..), fakeSession, WappState (..), Session (..), LiveWapp (..))
import Wapp.ApiTypes
import Wapp.UI.Auth as Wau
import qualified Native.Loader as Ld
import qualified Native.Registry as Nr

wappHandlers :: ToServant WappRoutes (AsServerT EasyVerseApp)
wappHandlers =
  genericServerT $ WappRoutes {
    phpTest = phpTestHdl
    , xStatic = xStaticHdl
    , wsStream = wsStreamInit
    , upload = uploadHdl
    , rootZN = welcomeHdZN
    , ui = wappUIHandlers
    , auth = wappAuthHandlers
    , user = wappUserHandlers
    , authz = wappAuthzHandlers
    , admin = wappAdminHandlers
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
  eiTargetApp <- getTargetApp rtOpts (T.unpack appID)

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
            (jsSession, jsModule) <-
              liftIO $ Jss.initJS (aLiveApp.wapp.rootPath </> T.unpack anIdent) firstLib.label
            pure . Just $ JSExecSupport jsSession jsModule ILib.buildNativeLibrary
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
  getTargetApp :: Rt.RunOptions -> String -> EasyVerseApp (Either String LiveWapp)
  getTargetApp rtOpts reqID = do
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
                newUpdateSignal <- liftIO Cs.newEmptyTMVarIO
                newWatcher <- liftIO $ newWatcher appDef.rootPath newCommChannel newUpdateSignal
                -- Get a DB pool for the app:
                mbDb <- case appDef.db of
                  Just dbConf ->
                    let
                      dbSettings = DbC.settings dbConf.host dbConf.port dbConf.user dbConf.passwd dbConf.dbase
                    in do
                    liftIO . putStrLn $ "@[wsStreamInit] acquiring db pool, " <> (T.unpack . decodeUtf8) dbConf.host <> ", user: " <>(T.unpack . decodeUtf8) dbConf.user <> ", db:" <> (T.unpack . decodeUtf8) dbConf.dbase
                    Just <$> liftIO (acquire dbConf.poolSize dbConf.acqTimeout dbConf.poolTimeOut dbConf.poolIdleTime dbSettings)
                  Nothing -> pure Nothing
                -- Load/link the native libraries for the app:
                liftIO $ P.putStrLn $ "@[wsStreamInit] wapp libs: " <> show appDef.libs
                forM_ appDef.libs $ \aLib -> do
                  case aLib.label of
                    "Natives" -> do
                      liftIO $ Ld.loadWappNative (Uu.toString aUID) rtOpts.nativesRoot (T.unpack aLib.ident)
                    _ -> pure $ Right ()

                let
                  newApp = LiveWapp {
                    wapp = appDef
                    , watcher = Just newWatcher
                    , signaler = newUpdateSignal
                    , commChannel = newCommChannel
                    , db = mbDb
                    }
                  newState = wState { cache = Mp.insert aUID newApp wState.cache }

                liftIO $ P.putStrLn $ "@[getTargetApp] adding new Wapp, ID: " <> show aUID
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
                        newContext = execCtxt { jsSupport = Just $ JSExecSupport jsSession jsModule ILib.buildNativeLibrary }
                      Rws.put newContext
                      case execCtxt.actionList of
                        [] -> pure ()
                        (hxMsg, anID, requestParams) : _ -> do
                          liftIO . putStrLn $ "@[clientIteration] replay last action."
                          rezA <- liftIO $ routeRequest refEnv newContext hxMsg anID requestParams
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
                                (modifiers, body, mbContainer) = case aReply of
                                  BasicFR (aHtml, mbText) -> ("", aHtml, mbText)
                                  AppendChildOOBFR (aHtml, mbText) -> ("hx-swap-oob=\"beforeend\"", aHtml, mbText)
                                  AfterEndOOBFR (aHtml, mbText) -> ("hx-swap-oob=\"afterend\"", aHtml, mbText)
                                response = case mbContainer of
                                  Nothing -> "<div" <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s) <> ">" <> body <> "</div>"
                                  Just customEle ->
                                    case customEle of
                                      "---" -> body
                                      _ -> "<" <> (Bs.fromStrict . encodeUtf8) customEle <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s) <> ">" <> body <> "</" <> (Bs.fromStrict . encodeUtf8) customEle <> ">"
                              in do
                              liftIO . putStrLn $ "@[clientIteration] customEle: " <> show mbContainer
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
                      requestParams = RequestParams { hxParamsRP = hxMsg.headers.params, formFieldsRP = hxMsg.formFields }
                    in do
                      let
                        newAction = (hxMsg, anID, requestParams)
                      Rws.modify $ \execCtxt -> execCtxt { actionList = newAction : execCtxt.actionList }
                      liftIO $ putStrLn $ "@[processIncomingMessage] requestParams: " <> show requestParams
                      rezA <- liftIO $ routeRequest refEnv execCtxt hxMsg anID requestParams
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
                            (modifiers, body, mbContainer) = case aReply of
                              BasicFR (aHtml, mbText) -> ("", aHtml, mbText)
                              AppendChildOOBFR (aHtml, mbText) -> ("hx-swap-oob=\"beforeend\"", aHtml, mbText)
                              AfterEndOOBFR (aHtml, mbText) -> ("hx-swap-oob=\"afterend\"", aHtml, mbText)
                            response = case mbContainer of
                                  Nothing -> "<div" <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s) <> ">" <> body <> "</div>"
                                  Just customEle -> case customEle of
                                    "---" -> "<div" <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s)
                                          <> " hx-swap=\"outerHTML\" hx-swap-oob=\"beforeend\">" <> body <> "</div>"
                                    _ -> "<" <> (Bs.fromStrict . encodeUtf8) customEle <> (case Bs.unwords [target, modifiers] of " " -> ""; s -> " " <> s) <> ">" <> body <> "</" <> (Bs.fromStrict . encodeUtf8) customEle <> ">"
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


uploadHdl :: MultipartData Tmp -> EasyVerseApp String
uploadHdl aMultipartData = do
  liftIO . putStrLn $ "@[uploadHdl] inputs: " <> show aMultipartData.inputs
  -- liftIO . putStrLn $ "@[uploadHdl] files: " <> show aMultipartData.files
  forM_ aMultipartData.files $ \aFile -> do
    liftIO . putStrLn $ "@[uploadHdl] inputName: " <> show aFile.fdInputName
        <> ", fileName: " <> show aFile.fdFileName
        -- <> ", size: " <> show (Lbs.length aFile.fdPayload)
    uName <- liftIO Uu.nextRandom
    let
      targetFile = "//bwork/wrkspc/karlin/tmp/EwAssets/" <> T.unpack (Uu.toText uName)
    liftIO . putStrLn $ "@[uploadHdl] dest name: " <> targetFile
    -- liftIO $ Lbs.writeFile targetFile aFile.fdPayload
    liftIO $ copyFileWithMetadata aFile.fdPayload targetFile
  -- Need to store the file info in the DB, and notify the client's session about the
  -- existence of the new files.
  pure "OK"

xStaticHdl :: [String] -> EasyVerseApp Html
xStaticHdl segments = do
  rtOpts <- Rws.asks config_Ctxt
  let
    -- Used to have the "/xstatic/" path in between root & segments.
    targetFile = rtOpts.zb.zbRootPath </> L.intercalate "/" segments
  liftIO . putStrLn $ "@[xStaticHandler] pageUrl: " <> targetFile
  pageContent <- liftIO $ Lbs.readFile targetFile
  pure . Html . Lbs.toStrict $ pageContent


{-
wappHandlers :: ToServant WappRoutes (AsServerT EasyVerseApp)
wappHandlers =
  genericServerT $ WappRoutes {
    phpTest = phpTestHdl
    , xStatic = xStaticHdl
    , wsStream = wsStreamInit
    , upload = uploadHdl
    , rootZN = welcomeHdZN
    , auth = wappAuthHandlers
    , user = wappUserHandlers
    , authz = wappAuthzHandlers
    , admin = wappAdminHandlers
  }
-}


-- ==========================================================================
-- Auth / User / Authz / Admin route handlers (skeletons)
-- ==========================================================================

wappUIHandlers :: ToServant WappUIRoutes (AsServerT EasyVerseApp)
wappUIHandlers =
  genericServerT $
    WappUIRoutes
      { auth = wappUIAuthHandlers
      }

wappUIAuthHandlers :: ToServant WappUIAuthRoutes (AsServerT EasyVerseApp)
wappUIAuthHandlers =
  genericServerT $
    WappUIAuthRoutes
      { signinPg = signinPgHdl
      , signupPg = signupPgHdl
      }

wappAuthHandlers :: ToServant WappAuthRoutes (AsServerT EasyVerseApp)
wappAuthHandlers = genericServerT $ WappAuthRoutes { 
        signin = signinHdl
      , signup = signupHdl
      , logout = logoutHdl
      , renew = renewHdl
      , oauthStart = oauthStartHdl
      , oauthCallback = oauthCallbackHdl
      }

wappUserHandlers :: ToServant WappUserRoutes (AsServerT EasyVerseApp)
wappUserHandlers =
  genericServerT $
    WappUserRoutes
      { accountPg = accountPgHdl
      , securityPg = securityPgHdl
      , me = meHdl
      , updateMe = updateMeHdl
      , sessions = sessionsHdl
      , revokeSession = revokeSessionHdl
      }

wappAuthzHandlers :: ToServant WappAuthzRoutes (AsServerT EasyVerseApp)
wappAuthzHandlers =
  genericServerT $
    WappAuthzRoutes
      { myPerms = myPermsHdl
      }

wappAdminHandlers :: ToServant WappAdminRoutes (AsServerT EasyVerseApp)
wappAdminHandlers =
  genericServerT $
    WappAdminRoutes
      { users = wappAdminUsersHandlers
      , wapps = wappAdminWappsHandlers
      }

wappAdminUsersHandlers :: ToServant WappAdminUsersRoutes (AsServerT EasyVerseApp)
wappAdminUsersHandlers =
  genericServerT $
    WappAdminUsersRoutes
      { listUsers = listUsersHdl
      , createUser = createUserHdl
      , getUser = getUserHdl
      , setUserStatus = setUserStatusHdl
      }

wappAdminWappsHandlers :: ToServant WappAdminWappsRoutes (AsServerT EasyVerseApp)
wappAdminWappsHandlers =
  genericServerT $
    WappAdminWappsRoutes
      { members = wappAdminWappMembersHandlers
      }

wappAdminWappMembersHandlers :: Uu.UUID -> ToServant WappAdminWappMembersRoutes (AsServerT EasyVerseApp)
wappAdminWappMembersHandlers _wappId =
  genericServerT $
    WappAdminWappMembersRoutes
      { listMembers = listMembersHdl
      , addMember = addMemberHdl
      , setMemberRole = setMemberRoleHdl
      , removeMember = removeMemberHdl
      }


-- --------------------------------------------------------------------------
-- UI handlers
-- --------------------------------------------------------------------------

type UiAuthHandler = Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> EasyVerseApp Html
signinPgHdl :: UiAuthHandler
signinPgHdl = fetchUIWithApp Wau.signinPanel

signupPgHdl :: UiAuthHandler
signupPgHdl = fetchUIWithApp Wau.signupPanel


fetchUIWithApp :: (String -> String -> String -> String) -> 
        Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> EasyVerseApp Html
fetchUIWithApp uiRender mbLang mbEwh mbApp mbNStep = do
  case mbApp of
    Nothing -> do
      simpleHtml "<html><body><h2>Error</h2><p>App is missing.</p></body></html>"
    Just appID -> do
      rtOpts <- Rws.asks config_Ctxt
      let
        -- hostHeader = lookup hHost (requestHeaders req)
        ewHost = maybe ("http://" <> T.unpack rtOpts.serverHost <> ":" <> show rtOpts.serverPort) T.unpack mbEwh
        nextStep = maybe "/" T.unpack mbNStep
      simpleHtml $ uiRender (T.unpack appID) ewHost nextStep


-- --------------------------------------------------------------------------
-- Auth handlers
-- --------------------------------------------------------------------------

loginPgHdl :: EasyVerseApp Html
loginPgHdl = do
  simpleHtml "<html><body><h2>Login</h2><p>TODO: handle proper login sid/cookie.</p></body></html>"

signinHdl :: SigninReq -> EasyVerseApp LoginReply
signinHdl req = do
  case req.wappID of
    Just wid -> do
      cid <- liftIO Uu.nextRandom
      uid <- liftIO Uu.nextRandom
      let
        appLanding = "/app-routing?wappID=" <> Uu.toText wid <> "&cid=" <> Uu.toText cid <> "&uid=" <> Uu.toText uid
      throwError $ RedirectAE appLanding "moving to app"
    Nothing -> do
      -- TODO: send back some error to the signin form.
      sid <- liftIO Uu.nextRandom
      cid <- liftIO Uu.nextRandom
      uid <- liftIO Uu.nextRandom
      pure $ LoginReply
        { sid = Uu.toText sid
        , cid = cid
        , userId = uid
        , displayName = Just "User"
        }


signupHdl :: SignupReq -> EasyVerseApp LoginReply
signupHdl req = do
  sidTxt <- case req.wappID of
    Just wid -> pure (Uu.toText wid)
    Nothing -> do
      s <- liftIO Uu.nextRandom
      pure (Uu.toText s)
  cid <- liftIO Uu.nextRandom
  uid <- liftIO Uu.nextRandom
  throwError $ RedirectAE "/new-page" "moving to app"


logoutHdl :: LogoutReq -> EasyVerseApp NoContent
logoutHdl _req = do
  -- TODO: revoke session in DB
  pure NoContent

renewHdl :: RenewReq -> EasyVerseApp RenewReply
renewHdl req = do
  -- TODO: rotate session token, re-attach to context
  -- For now: mint a new sid (UUID text) and keep cid as provided.
  newSid <- liftIO Uu.nextRandom
  pure $
    RenewReply
      { sid = Uu.toText newSid
      , cid = req.cid
      }

oauthStartHdl :: Text -> Maybe Text -> EasyVerseApp Html
oauthStartHdl provider mbNext = do
  let nextTxt = Mb.fromMaybe "/wbap" mbNext
  simpleHtml $
    "<html><body><h2>OAuth start</h2><p>provider: "
      <> Bs.unpack (Bs.fromStrict (encodeUtf8 provider))
      <> "</p><p>next: "
      <> Bs.unpack (Bs.fromStrict (encodeUtf8 nextTxt))
      <> "</p><p>TODO: redirect to provider with PKCE.</p></body></html>"

oauthCallbackHdl :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> EasyVerseApp Html
oauthCallbackHdl provider mbCode mbState mbErr = do
  simpleHtml $
    "<html><body><h2>OAuth callback</h2><p>provider: "
      <> Bs.unpack (Bs.fromStrict (encodeUtf8 provider))
      <> "</p><p>code: "
      <> show mbCode
      <> "</p><p>state: "
      <> show mbState
      <> "</p><p>error: "
      <> show mbErr
      <> "</p><p>TODO: exchange code, validate id_token, create/link user, create sid.</p></body></html>"


-- --------------------------------------------------------------------------
-- User handlers
-- --------------------------------------------------------------------------

simpleHtml :: String -> EasyVerseApp Html
simpleHtml s = pure . Html . Lbs.toStrict $ Bs.pack s


accountPgHdl :: EasyVerseApp Html
accountPgHdl =
  simpleHtml
    "<html><body><h2>Account</h2><p>TODO: render account page (SSR) using principal from sid/cookie.</p></body></html>"

securityPgHdl :: EasyVerseApp Html
securityPgHdl =
  simpleHtml
    "<html><body><h2>Security</h2><p>TODO: sessions, password change, MFA/passkeys.</p></body></html>"

meHdl :: Maybe Text -> EasyVerseApp UserSelf
meHdl _mbSid = do
  uid <- liftIO Uu.nextRandom
  pure $
    UserSelf
      { userId = uid
      , primaryEmail = Nothing
      , displayName = Just "User"
      , avatarUrl = Nothing
      }

updateMeHdl :: Maybe Text -> UpdateUserSelfReq -> EasyVerseApp UserSelf
updateMeHdl _mbSid req = do
  uid <- liftIO Uu.nextRandom
  pure $
    UserSelf
      { userId = uid
      , primaryEmail = Nothing
      , displayName = req.displayName
      , avatarUrl = req.avatarUrl
      }

sessionsHdl :: Maybe Text -> EasyVerseApp [SessionInfo]
sessionsHdl _mbSid = do
  -- TODO: list sessions for current user
  pure []

revokeSessionHdl :: Text -> Maybe Text -> EasyVerseApp NoContent
revokeSessionHdl _sidToRevoke _mbSid = do
  -- TODO: revoke a given sid for current user
  pure NoContent


-- --------------------------------------------------------------------------
-- Authz handlers
-- --------------------------------------------------------------------------

myPermsHdl :: Maybe Text -> Maybe Uu.UUID -> EasyVerseApp PermissionSet
myPermsHdl _mbSid mbWappId = do
  pure $
    PermissionSet
      { wappId = mbWappId
      , roles = []
      , permissions = []
      }


-- --------------------------------------------------------------------------
-- Admin handlers
-- --------------------------------------------------------------------------

listUsersHdl :: EasyVerseApp [AdminUserSummary]
listUsersHdl = pure []

createUserHdl :: AdminCreateUserReq -> EasyVerseApp AdminUserSummary
createUserHdl req = do
  uid <- liftIO Uu.nextRandom
  pure $
    AdminUserSummary
      { userId = uid
      , primaryEmail = Just req.email
      , displayName = req.displayName
      , status = "active"
      }

getUserHdl :: Uu.UUID -> EasyVerseApp AdminUserSummary
getUserHdl uid =
  pure $
    AdminUserSummary
      { userId = uid
      , primaryEmail = Nothing
      , displayName = Nothing
      , status = "active"
      }

setUserStatusHdl :: Uu.UUID -> AdminSetUserStatusReq -> EasyVerseApp AdminUserSummary
setUserStatusHdl uid req =
  pure $
    AdminUserSummary
      { userId = uid
      , primaryEmail = Nothing
      , displayName = Nothing
      , status = req.status
      }

listMembersHdl :: EasyVerseApp [WappMemberSummary]
listMembersHdl = pure []

addMemberHdl :: AddWappMemberReq -> EasyVerseApp WappMemberSummary
addMemberHdl req = do
  pure $
    WappMemberSummary
      { userId = req.userId
      , role = req.role
      , addedAt = "now"
      }

setMemberRoleHdl :: Uu.UUID -> SetWappMemberRoleReq -> EasyVerseApp WappMemberSummary
setMemberRoleHdl uid req = do
  pure $
    WappMemberSummary
      { userId = uid
      , role = req.role
      , addedAt = "now"
      }

removeMemberHdl :: Uu.UUID -> EasyVerseApp NoContent
removeMemberHdl _uid = pure NoContent
