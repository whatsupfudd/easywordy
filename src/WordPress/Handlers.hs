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


import Options.Runtime (RunOptions (..), WpConfig (..), ZbConfig (..))

import Api.Types
import qualified HttpSup.Types as Ht
import WordPress.RouteDef (WpTopRoutes (..), AdminRoutes (..), IncludesRoutes (..)
      , IndexPosting (..), PostComment (..), Trackback (..), InstallPost (..))
import WordPress.Wrapper (handlePhpRequest)


wordpressHandlers :: ToServant WpTopRoutes (AsServerT EasyVerseApp)
wordpressHandlers =
  genericServerT $ WpTopRoutes {
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
  }


adminHandlers :: ToServant AdminRoutes (AsServerT EasyVerseApp)
adminHandlers = 
  genericServerT $ AdminRoutes {
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
  (aStr, duration) <- liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET, uri = targetUrl, queryArgs = argMap, reqHeaders = [], reqBody = Nothing })
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
            liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
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
              liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
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
              liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
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
      liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
                , uri = "/wp-admin/install.php", queryArgs = Mp.empty
                , reqHeaders = [], reqBody = Nothing })
  pure $ Html result


installHandler :: Maybe (Either Text Int) -> InstallPost -> EasyVerseApp Html
installHandler mbStepID reqData = do
  _ <- liftIO . putStrLn $ "@[installHandler] mbStepID: " <> show mbStepID
  rtOpts <- asks config_Ctxt
  (result, duration) <- case mbStepID of
    Nothing -> do
        liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
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
        liftIO $ handlePhpRequest rtOpts (Ht.Request { method = Ht.GET
              , uri = "/wp-admin/install.php", queryArgs = Mp.singleton "step" (show number)
              , reqHeaders = [], reqBody = Nothing })
  pure . Html $ result

