{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module WordPress.Handlers where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Control.Exception.Safe (tryAny)
import Control.Monad (forever, void)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as Bs
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Data.Map as Mp
import Data.Text (Text, pack, unpack, splitOn)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import System.FilePath.Posix ((</>))

import GHC.Generics

import qualified Network.WebSockets as WS

import Data.Aeson (FromJSON, ToJSON, Value (Object), (.:), (.:?), (.=), parseJSON, eitherDecode)

import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.WebSocket (WebSocket)
import Network.HTTP.Types.URI (QueryItem)

import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H

import Hasql.Pool (Pool)
import Wapp.DemoPage (demoPage, demoSearch, demoReply)
import Wapp.MockData (projectList)
import qualified Wapp.Router as Wr

import Options.Runtime (RunOptions (..), WpConfig (..), ZbConfig (..))

import Api.Types
import qualified HttpSup.Types as Ht
import Wapp.Router (RouteArg (..))
import WordPress.ApiTypes
import WordPress.Wrapper (handleRequest)


wordpressHandlers :: ToServant TopRoutesWP (AsServerT EasyVerseApp)
wordpressHandlers =
  genericServerT $ TopRoutesWP {
    admin = adminHandlers -- WP.adminHandler
    , includes = includesHandlers -- Includes.includesHandler
    , homePage = homeHandler -- WP.indexHandler
    , indexGet = indexHandler -- WP.indexHandler
    , indexPost = indexReactor -- WP.indexHandler
    , login = fakeWpHandler -- WP.loginHandler
    , signup = fakeWpHandler -- WP.signupHandler
    , xmlRpc = fakeWpHandler -- WP.xmlRpcHandler
    , trackback = fakeWpHandlerTrackBack -- WP.trackbackHandler
    , cron = cronHandler
    , easywordy = easywordyHandlers -- EasyWordy.easywordyHandler
  }


adminHandlers :: ToServant AdminRoutesWP (AsServerT EasyVerseApp)
adminHandlers = 
  genericServerT $ AdminRoutesWP {
    root = adminHandler "" Mp.empty
    , index = adminHandler "index.php" Mp.empty -- WP.adminHandler
    , postNew = fakeWpHandler -- WP.adminHandler
    , post = adminHandler "post.php" . Mp.singleton "p" . show -- WP.adminHandler
    , postPages = fakeWpHandlerMbText -- WP.adminHandler
    , postTags = fakeWpHandlerTextInt -- WP.adminHandler
    , upload = fakeWpHandler -- WP.adminHandler
    , mediaNew = fakeWpHandler -- WP.adminHandler
    , postComment = fakeWpHandlerPComment -- WP.adminHandler
    , themes = fakeWpHandler -- WP.adminHandler
    , siteEditor = fakeWpHandlerText -- WP.adminHandler
    , customize = fakeWpHandlerText -- WP.adminHandler
    , widgets = fakeWpHandler -- WP.adminHandler
    , themesInstall = fakeWpHandler -- WP.adminHandler
    , menus = fakeWpHandlerTextInt -- WP.adminHandler
    , plugins = fakeWpHandler -- WP.adminHandler
    , pluginsInstall = fakeWpHandler -- WP.adminHandler
    , users = fakeWpHandler -- WP.adminHandler
    , usersNew = fakeWpHandler -- WP.adminHandler
    , profile = fakeWpHandler -- WP.adminHandler
    , tools = fakeWpHandler -- WP.adminHandler
    , importExport = fakeWpHandler -- WP.adminHandler
    , export = fakeWpHandler -- WP.adminHandler
    , exportPersonalData = fakeWpHandler -- WP.adminHandler
    , erasePersonalData = fakeWpHandler -- WP.adminHandler
    , settings = fakeWpHandler -- WP.adminHandler
    , optionsGeneral = fakeWpHandler -- WP.adminHandler
    , optionsWriting = fakeWpHandler -- WP.adminHandler
    , optionsReading = fakeWpHandler -- WP.adminHandler
    , optionsDiscussion = fakeWpHandler -- WP.adminHandler
    , optionsMedia = fakeWpHandler -- WP.adminHandler
    , optionsPermalinks = fakeWpHandler -- WP.adminHandler
    , optionsPrivacy = fakeWpHandlerMbText -- WP.adminHandler
    , ajaxAdmin = fakeWpHandler -- WP.adminHandler
    , commentAdmin = fakeWpHandlerInt -- WP.adminHandler
    , installGet = startInstall -- WP.installHandler
    , installPost = installHandler -- WP.installHandler
  }

includesHandlers :: ToServant IncludesRoutes (AsServerT EasyVerseApp)
includesHandlers =
  genericServerT $ IncludesRoutes {
    catchAll = catchAllPathHandler
  }

catchAllPathHandler :: [String] -> [ QueryItem ] -> EasyVerseApp Html
catchAllPathHandler aPath aQuery = do
  rtOpts <- asks config_Ctxt
  let
    fullPath = rtOpts.wp.rootPath <> "/wp-includes/" <> L.intercalate "/" aPath
  -- _ <- liftIO . putStrLn $ "@[catchAllPathHandler] fullPath: " <> fullPath <> " query: " <> show aQuery
  fileContent <- liftIO $ Bs.readFile fullPath
  pure . Html $ fileContent

cronHandler :: Maybe (Either Text Text) -> EasyVerseApp Html
cronHandler mbDoingWpCron = do
  pure . Html $ "<html><head><title>EASY VERSE</title></head><body>Doing cron.</body></html>"

fakeWpHandler :: EasyVerseApp Html
fakeWpHandler = do
  _ <- liftIO $ putStrLn "@[fakeWpHandler]"
  pure . Html $ "<html><head><title>EASY VERSE</title></head><body>EASY VERSE, a WordPress extension for Fudd.<br/><br/>For more information, consult <a href='https://whatsupfudd.com'>FUDD</a></body></html>"

fakeWpHandlerText :: Text -> EasyVerseApp Html
fakeWpHandlerText aPath =
  fakeWpHandler

fakeWpHandlerInt :: Int -> EasyVerseApp Html
fakeWpHandlerInt aInt =
  fakeWpHandler

fakeWpHandlerMbText :: Maybe (Either Text Text) -> EasyVerseApp Html
fakeWpHandlerMbText aMbText =
  fakeWpHandler

fakeWpHandlerTextInt :: Text -> Int -> EasyVerseApp Html
fakeWpHandlerTextInt aTaxo aInt =
  fakeWpHandler

fakeWpHandlerPComment :: PostComment -> EasyVerseApp Html
fakeWpHandlerPComment aPostComment =
  fakeWpHandler

fakeWpHandlerTrackBack :: Trackback -> EasyVerseApp Html
fakeWpHandlerTrackBack aTrackBack =
  fakeWpHandler


adminHandler :: String -> Mp.Map String String -> EasyVerseApp Html
adminHandler aPath argMap = do
  rtOpts <- asks config_Ctxt
  let
    targetUrl = case aPath of
      "" -> "wp-admin/index.php"
      _ -> "wp-admin/" <> aPath
  (aStr, duration) <- liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET, uri = targetUrl, queryArgs = argMap, reqHeaders = [], reqBody = Nothing })
  pure . Html $ aStr


homeHandler ::
    Maybe (Either Text Int)  -- post id
    -> EasyVerseApp Html
homeHandler mbPostID = do
  _ <- liftIO $ putStrLn "@[homeHandler]"
  case mbPostID of
    Nothing ->
      pure . Html $ "<html><head><title>EASY VERSE</title></head><body> no post id.</body></html>"
    Just eiPostID ->
      case eiPostID of
        Left errMsg ->
          pure . Html $ "<html><head><title>EASY VERSE</title></head><body> postID param err: " <> encodeUtf8 errMsg <> "</body></html>"
        Right postID -> do
          rtOpts <- asks config_Ctxt
          (aStr, duration) <-
            liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
                    , uri = "index.php", queryArgs = Mp.singleton "p" (show postID)
                    , reqHeaders = [], reqBody = Nothing })
          pure . Html $ aStr


indexHandler :: Maybe (Either Text Int)  -- post id
    -> Maybe (Either Text Int)           -- page id
    -> Maybe (Either Text Int)           -- category id
    -> Maybe (Either Text Text)           -- tag
    -> Maybe (Either Text Text)           -- search term
    -> Maybe (Either Text Text)           -- date archive
    -> Maybe (Either Text Int)           -- author ID
    -> Maybe (Either Text Text)           -- post slug
    -> EasyVerseApp Html
indexHandler mbPostID mbPageID mbCatID mbTagName mbSearchTerm mbDate mbAuthorID mbPostSlug = do
  _ <- liftIO $ putStrLn "@[fakeWpHandler]"
  case mbPostID of
    Nothing ->
      pure . Html $ "<html><head><title>EASY VERSE</title></head><body> no page id.</body></html>"
    Just eiPostID ->
      case eiPostID of
        Left errMsg ->
          pure . Html $ "<html><head><title>EASY VERSE</title></head><body> pageID param err: " <> encodeUtf8 errMsg <> "</body></html>"
        Right postID -> do
          rtOpts <- asks config_Ctxt
          (aStr, duration) <-
              liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
                    , uri = "index.php", queryArgs = Mp.singleton "p" (show postID)
                    , reqHeaders = [], reqBody = Nothing })
          pure . Html $ aStr


indexReactor :: Maybe (Either Text Int) -> IndexPosting -> EasyVerseApp Html
indexReactor mbStepID reqData = do
  _ <- liftIO . putStrLn $ "@[indexReactor] reqData: " <> show reqData
  rtOpts <- asks config_Ctxt
  (result, duration) <- case mbStepID of
      Nothing -> pure ("<html><head><title>EASY VERSE</title></head><body><nil></body></html>", 0)
      Just eiNumber -> case eiNumber of
        Left errMsg ->
          let msg = "<html><head><title>EASY VERSE</title></head><body>err: " <> encodeUtf8 errMsg <> "</body></html>"
          in pure (msg, 0)
        Right aVal -> do
          (result, duration) <-
              liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
                    , uri = "index.php", queryArgs = Mp.singleton "step" (show aVal)
                    , reqHeaders = [], reqBody = Nothing })
          pure (result, duration)
  {-
    resultPost =
      case reqData of
        Language l -> encodeUtf8 l.language

    Il faut ensuite composer la requete pour php avec:
      - POST qui receoit un array de valeurs, dans ce cas-ci: { "language" : "" }
      - GET qui recoit aussi les valeurs, dans ce cas-ci { "step" : "1" }
  -}
  pure $ Html result

startInstall :: EasyVerseApp Html
startInstall = do
  rtOpts <- asks config_Ctxt
  (result, duration) <-
      liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
                , uri = "/wp-admin/install.php", queryArgs = Mp.empty
                , reqHeaders = [], reqBody = Nothing })
  pure $ Html result


installHandler :: Maybe (Either Text Int) -> InstallPost -> EasyVerseApp Html
installHandler mbStepID reqData = do
  _ <- liftIO . putStrLn $ "@[installHandler] mbStepID: " <> show mbStepID
  rtOpts <- asks config_Ctxt
  (result, duration) <- case mbStepID of
    Nothing -> do
        liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
              , uri = "/wp-admin/install.php", queryArgs = Mp.empty
              , reqHeaders = [], reqBody = Nothing })
    Just eiNumber -> case eiNumber of
      Left errMsg ->
        let
          msg = "<html><head><title>EASY VERSE</title></head><body>err: " <> encodeUtf8 errMsg <> "</body></html>"
        in
        pure (msg, 0)
      Right number -> do
        liftIO . putStrLn $ "@[installHandler] number: " <> show number
        liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
              , uri = "/wp-admin/install.php", queryArgs = Mp.singleton "step" (show number)
              , reqHeaders = [], reqBody = Nothing })
  pure . Html $ result

-- EasyWordy handlers:

easywordyHandlers :: ToServant EasyWordyRoutes (AsServerT EasyVerseApp)
easywordyHandlers =
  genericServerT $ EasyWordyRoutes {
    rootZP = welcomeHdZP
    , indexZP = indexHdZP
    , demoWs = testWS -- WP.demoWsHandler
    , demoSrch = demoSearchHandler -- WP.demoSearchHandler
    , xStatic = xStaticHandler -- WP.xStaticHandler
    , wsStream = wsStreamHandler -- WP.wsStreamHandler
  }

welcomeHdZP :: EasyVerseApp Html
welcomeHdZP = do
  rtOpts <- asks config_Ctxt
  let
    targetFile = rtOpts.zb.zbRootPath <> "/mainFrame_1.html"
  content <- liftIO $ Bs.readFile targetFile
  pure . Html $ content

indexHdZP :: Maybe (Either Text Int) -> EasyVerseApp Html
indexHdZP mbPostID = do
  _ <- liftIO . putStrLn $ "@[indexHdZP] mbPostID: " <> show mbPostID
  rtOpts <- asks config_Ctxt
  let
    targetUrl = "zhpr/index.php"
    argMap = case mbPostID of
      Nothing -> Mp.empty :: Mp.Map String String
      Just (Left errMsg) -> Mp.singleton "err" (unpack errMsg)
      Just (Right postID) -> Mp.singleton "p" (show postID)
  (aStr, duration) <-
        liftIO $ handleRequest rtOpts (Ht.Request { method = Ht.GET
              , uri = targetUrl, queryArgs = argMap
              , reqHeaders = [], reqBody = Nothing })
  pure . Html $ aStr


-- MonadIO m => WS.Connection -> m ()
wsStreamHandler :: WS.Connection -> EasyVerseApp ()
wsStreamHandler conn = do
  rtOpts <- asks config_Ctxt
  pgDb <- asks pgPool_Ctxt
  liftIO $ WS.withPingThread conn 30 (pure ()) $ do
    -- liftIO $ WS.sendTextData conn ("<div id=\"notifications\" hx-swap-oob=\"beforeend\">Some message</div?" :: ByteString)
    -- TODO: figure out the routing-table refresh mechanism that doesn't require a websocket reconnection.
    handleClient rtOpts pgDb (Wr.fakeRoutingInit rtOpts)
  where
  handleClient :: RunOptions -> Pool -> Wr.RoutingTable -> IO ()
  handleClient rtOpts pgDb routingTable = do
    rezA <- tryAny $ forever (receiveStream rtOpts pgDb routingTable)
    case rezA of
      Left err -> do
        liftIO . putStrLn $ "@[streamHandler] situation: " <> show err
        closeConnection
      Right _ -> do
        liftIO $ putStrLn "@[streamHandler] client disconnected."
        pure ()

  receiveStream :: RunOptions -> Pool -> Wr.RoutingTable -> IO ()
  receiveStream rtOpts pgDb routingTable = do
    rezA <- WS.receiveDataMessage conn
    case rezA of
      WS.Text msg decodedMsg ->
        let
          hxMsg = eitherDecode msg :: Either String HxWsMessage
        in
        case hxMsg of
          Left err -> do
            putStrLn $ "@[receiveStream] invalid HxWsMessage: " <> (unpack . decodeUtf8 . LBS.toStrict) msg
            putStrLn $ "@[receiveStream] error: " <> show err
          Right hxMsg ->
            case hxMsg.wsMessage of
              Nothing ->
                case hxMsg.headers.mid of
                  Nothing ->
                    WS.sendTextData conn ("<div id=\"mainContainer\"></div>" :: Bs.ByteString)
                  Just anID -> do
                    let
                      argMap = case hxMsg.headers.args of
                        Nothing -> Mp.empty
                        Just args -> Mp.fromList $ map (\argValue -> case splitOn "=" argValue of
                          [key, value] -> (key, TextRA value)
                          _ -> ("", IntRA 0)) $ splitOn "&" args
                    -- TODO: need to extract the argMap from the ws-messsage.
                    rezA <- Wr.routeRequest rtOpts pgDb routingTable anID argMap
                    case rezA of
                      Left errMsg -> do
                        putStrLn $ "@[receiveStream] routeRequest error: " <> show errMsg
                        WS.sendTextData conn ("<div id=\"mainContainer\"></div>" :: Bs.ByteString)
                      Right aHtml -> do
                        WS.sendTextData conn aHtml
              Just aText ->
                WS.sendTextData conn $ H.renderHtml $ demoReply hxMsg.wsMessage
      WS.Binary msg ->
        putStrLn "@[receiveStream] received binary."
  
  closeConnection = do
    WS.sendClose conn ("Bye" :: ByteString)
    void $ WS.receiveDataMessage conn


data HxWsHeaders = HxWsHeaders {
    request :: Text
    , trigger :: Maybe Text
    , triggerName :: Maybe Text
    , target :: Text
    , currentURL :: Text
    , mid :: Maybe Text
    , args :: Maybe Text
  }
  deriving stock (Show, Generic)


instance FromJSON HxWsHeaders where
  parseJSON (Object obj) = HxWsHeaders <$>
    obj .: "HX-Request"
    <*> obj .:? "HX-Trigger"
    <*> obj .:? "HX-Trigger-Name"
    <*> obj .: "HX-Target"
    <*> obj .: "HX-Current-URL"
    <*> obj .:? "mid"
    <*> obj .:? "args"


data HxWsMessage = HxWsMessage {
    wsMessage :: Maybe Text
    , headers :: HxWsHeaders
  }
  deriving (Show, Generic)


instance FromJSON HxWsMessage where
  parseJSON (Object obj) = HxWsMessage <$>
    obj .:? "hxid-1"
    <*> obj .: "HEADERS"

-- TMP DEMO:
demoSearchHandler :: SearchContent -> EasyVerseApp Html
demoSearchHandler needle = do
  pure . Html . LBS.toStrict . H.renderHtml $ demoSearch projectList needle.search

testWS :: EasyVerseApp Html
testWS = do
  pure . Html . LBS.toStrict . H.renderHtml $ demoPage "First Test Page WS." []

xStaticHandler :: String -> EasyVerseApp Html
xStaticHandler pageUrl = do
  liftIO . putStrLn $ "@[xStaticHandler] pageUrl: " <> pageUrl
  pageContent <- liftIO $ LBS.readFile ("xstatic/" <> pageUrl)
  pure . Html . LBS.toStrict $ pageContent
