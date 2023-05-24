module WordPress.Handlers where

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Api.Types
import WordPress.ApiTypes
import WordPress.Wrapper (testPHP)


wordpressHandlers :: ToServant WordPressRoutes (AsServerT EasyVerseApp)
wordpressHandlers =
  genericServerT $ WordPressRoutes {
    login = fakeWpHandler -- WP.loginHandler
    , admin = fakeWpHandlerWithPath -- WP.adminHandler
    , cron = fakeWpHandler -- WP.cronHandler
    , indexGet = indexHandler -- WP.indexHandler
    , indexPost = indexReactor -- WP.indexHandler
    , homePage = fakeWpHandler -- WP.indexHandler
  }


fakeWpHandler :: EasyVerseApp Html
fakeWpHandler = do
  _ <- liftIO $ putStrLn "@[fakeWpHandler]"
  pure . Html $ "<html><head><title>EASY VERSE</title></head><body>EASY VERSE, a WordPress extension for Fudd.<br/><br/>For more information, consult <a href='https://whatsupfudd.com'>FUDD</a></body></html>"


fakeWpHandlerWithPath :: String -> EasyVerseApp Html
fakeWpHandlerWithPath aPath =
  fakeWpHandler


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
        Right postID ->
          let
            bsPostID = encodeUtf8 . pack $ show postID
          in do
          aStr <- liftIO testPHP
          {- pure . Html $ "<html><head><title>EASY VERSE</title></head><body> showing page ID: "
                <> aStr <> "</body></html>"
          -}
          pure . Html $ aStr


indexReactor :: Maybe (Either Text Int) -> IndexPosting -> EasyVerseApp Html
indexReactor mbStepID reqData = do
  _ <- liftIO . putStrLn $ "@[indexReactor] reqData: " <> show reqData
  let
    step = case mbStepID of
      Nothing -> "<nil>"
      Just eiNumber -> case eiNumber of
        Left errMsg -> "err: " <> encodeUtf8 errMsg
        Right aVal -> "s: " <> (encodeUtf8 . pack $ show aVal)
        
    resultPost =
      case reqData of
        Language l -> encodeUtf8 l.language
  {-
    Il faut ensuite composer la requete pour php avec:
      - POST qui receoit un array de valeurs, dans ce cas-ci: { "language" : "" }
      - GET qui recoit aussi les valeurs, dans ce cas-ci { "step" : "1" }
  -}
  pure . Html $ "step: " <> step <> ", body: " <> resultPost
  