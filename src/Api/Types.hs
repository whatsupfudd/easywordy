{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api.Types where


import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, withExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
-- import Control.Monad.RWS.Lazy (RWST, MonadReader, MonadState)

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
-- import Data.Set (Set)

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Servant.Auth.Server (Auth, AuthResult
          , JWT, JWTSettings, FromJWT, ToJWT
          , BasicAuth, BasicAuthData, BasicAuthCfg, FromBasicAuthData (..)
        )
import Servant.Server
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic ((:-), ToServantApi, ToServant)
import Servant.API ((:>), ReqBody, JSON, Capture, Post, Get, Accept (..), MimeRender (..))
import Network.HTTP.Media ((//), (/:))

import Hasql.Pool (Pool)
import Database.MySQL.Base (MySQLConn)

import Foreign (Ptr)

import qualified Options.Runtime as Rt
-- import Wapp.Types (RoutingDictionary)
import qualified Wapp.State as Ws
import qualified Wapp.FileWatcher as Wf
import qualified Assets.Types as S3

-- Client Data going in / out.
data ClientInfo = ClientInfo {
    sessionID :: Int32
    , expiry :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToJWT ClientInfo
instance FromJWT ClientInfo
instance FromBasicAuthData ClientInfo where
  fromBasicAuthData authData authChecker = authChecker authData


data LoginForm = LoginForm { username :: Text, secret :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


newtype LoginRequest =
  SimpleLR LoginForm


data SessionItems = SessionItems {
    sessionCtxt :: ClientInfo
    , jwt :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)


data LoginResult =
  ErrorLR String
  | AuthenticatedLR SessionItems
  deriving stock Generic
  deriving anyclass (ToJSON)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult ClientInfo)


data HTML = HTML
instance Accept HTML where
   contentType _ = "text" // "html" /: ("charset", "utf-8")
 

newtype Html = Html { html :: ByteString }

instance MimeRender HTML Html where
  mimeRender _ = LBS.fromStrict . html


-- Context to all API handlers (ReaderT)

data AppEnv = AppEnv {
    config_Ctxt :: Rt.RunOptions
    , jwt_Ctxt :: JWTSettings
    , pgPool_Ctxt :: Pool
    , mqlConn_Ctxt :: MySQLConn
    , sapiModuleDef_Ctxt :: Ptr ()
    , appDefWatcher_Ctxt :: Maybe Wf.WatcherControl
    , state_Tmp :: Ws.WappState
    , s3Storage_Ctxt :: Maybe S3.S3Conn
  }


-- Error management:
data ApiError =
  DoesNotExistAE Text
  | InternalErrorAE Text
  | UnauthorizedAE
  | UnknownSituationAE Text
  | RedirectAE Text Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


asServantError :: ApiError -> ServerError
asServantError error =
  case error of
    DoesNotExistAE  msg-> err404 { errBody = LBS.fromStrict . encodeUtf8 $ msg }
    InternalErrorAE msg -> err500 { errBody = LBS.fromStrict . encodeUtf8 $ msg }
    UnauthorizedAE -> err401 { errBody = "Unauthorized access." }
    UnknownSituationAE msg -> err500 { errBody = LBS.fromStrict . encodeUtf8 $ msg }
    RedirectAE location msg -> err303 { errBody = LBS.fromStrict . encodeUtf8 $ msg, errHeaders = [("Location", encodeUtf8 location)] }


-- Main Application / handler for the API:
newtype EasyVerseApp api = EasyVerseApp {
     -- apiHandler :: RWST AppEnv (Set Ws.ErrMessage) Ws.WappState (ExceptT ApiError IO) api
     apiHandler :: ReaderT AppEnv (ExceptT ApiError IO) api
  }
  deriving newtype ( 
      Functor,
      Applicative,
      Monad,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadReader AppEnv,
      MonadIO,
      MonadError ApiError
      -- , MonadState Ws.WappState
    )


newtype AuthenticatedRoutes route = AuthenticatedRoutes {
    -- TODO: generalize to get-any-asset.
    getPage :: route :- ToServantApi GetPageRoutes
  }
  deriving stock (Generic)


-- Need to match the Cors publicPrefixes.
data AnonymousRoutes route = AnonymousRoutes { 
    login :: route :- "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] LoginResult
    , anonGetPage :: route :- "s" :> Capture "path" String :> Get '[HTML] Html
  }
  deriving stock (Generic)


newtype GetPageRoutes route = GetPageRoutes { 
    getPage :: route :- "p" :> Capture "path" String :> Get '[HTML] Html
  }
  deriving stock (Generic)

