{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}


module Api.Handlers where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (runContT, ContT (..))
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Int as DI
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.List.NonEmpty (NonEmpty (..))

import Data.Aeson (FromJSON (..), ToJSON (..))

import qualified Network.Wai.Handler.Warp as Wrp
import Network.HTTP.Media ((//), (/:))

-- Incompatible with aeson-2
-- import Network.Wai.Middleware.Servant.Errors (errorMw)

import GHC.Generics
-- import GHC.Stack (HasCallStack)
import Servant as Srv

import Data.Streaming.Network.Internal (HostPreference (..))

import Servant.Auth.Server (AuthResult (..))
import Servant.Server.Generic (AsServerT, genericServerT)
-- import Hasql.Pool (Pool)
import System.Posix.Signals as Sgnl

import Options.Runtime as Ropt
import Api.Session (loginUser)
import Api.Types


-- TODO: move this into HttpSup section:
setupWai :: Int -> DT.Text -> IO () -> Wrp.Settings
setupWai port host shutdownCallback =
  Wrp.setPort port . Wrp.setHost (Host (DT.unpack host)) . Wrp.setGracefulShutdownTimeout (Just 5) . Wrp.setInstallShutdownHandler shutdownHandler
    -- . setBeforeMainLoop showBanner
    $ Wrp.defaultSettings
  where
    shutdownHandler closeSocket = do
      void $ installHandler Sgnl.sigTERM (Catch $ shutdownCallback >> closeSocket) Nothing
      void $ installHandler Sgnl.sigINT (Catch $ shutdownCallback >> closeSocket) Nothing


-- Anonymous Handlers:
anonHandlers :: ToServant AnonymousRoutes (AsServerT EasyVerseApp)
anonHandlers =
  genericServerT $ AnonymousRoutes {
    login = loginHandler
    , anonGetPage = anonGetPageHandler
    -- , homePage = homePageHandler
  }


loginHandler :: LoginForm -> EasyVerseApp LoginResult
loginHandler form = do
  liftIO $ putStrLn $ "@[loginHandler] user:" <> DT.unpack form.username <> ", pwd: " <> DT.unpack form.secret
  -- TODO: check for u/p validity, if so return new Session, otherwise throw error:
  loginUser form
  {-
    -- Session.loginUser form
    --  LoginResult {
      context = (ClientInfo 1 <time>)
      , jwt = (decodeUtf8 . LBS.toStrict $ "<fake-jwt>")
    --

    let
      mbRez = Nothing
    case mbRez of
      Nothing -> throwError UnauthorizedAE
                    -- $ "Invalid login credentials for : " <> DT.unpack form.username
      Just aResult -> pure aResult
  -}


anonGetPageHandler :: String -> EasyVerseApp Html
anonGetPageHandler pageUrl = do
  _ <- liftIO $ putStrLn "@[anonGetPageHandler]"
  let
    tmpStr = encodeUtf8 . DT.pack $ pageUrl
  pageContent <- liftIO $ LBS.readFile ("/tmp/" <>pageUrl)
  pure . Html $ "<html><head><title>TEST</title></head><body>TEST: " <> LBS.toStrict pageContent <> "</body></html>"


homePageHandler :: EasyVerseApp Html
homePageHandler = do
  _ <- liftIO $ putStrLn "@[homePageHandler]"
  pure . Html $ "<html><head><title>EASY VERSE/title></head><body>Welcome to EasyVerse, the site starts at <a href='/site/landing.html'>Landing</a><br/><br/>For more information, consult <a href='https://whatsupfudd.com'>FUDD</a></body></html>"


instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Html where
  mimeRender _ = LBS.fromStrict . html


-- Protected routes:
authHandlers :: AuthResult ClientInfo -> ToServant AuthenticatedRoutes (AsServerT EasyVerseApp)
authHandlers authResult = genericServerT $ AuthenticatedRoutes {
    getPage = getPageHandler authResult
  }


getPageHandler :: AuthResult ClientInfo -> String -> EasyVerseApp Html
getPageHandler authResult pageUrl = do
  authCtxt <- fromAuthResult authResult
  -- Getting here means the authResult is valid.
  pure . Html $ "<html><head><title>TEST AUTH</title></head><body>" <> (encodeUtf8 . DT.pack . show $ authCtxt) <> "</body></html>"



fromAuthResult :: AuthResult a -> EasyVerseApp a
fromAuthResult (Authenticated uid) = return uid
fromAuthResult x =
  throwError UnauthorizedAE
    -- . DT.pack . show $ x
