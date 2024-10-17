{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Types where


import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, withExceptT, MonadIO, MonadError)
import Control.Monad.Reader (ReaderT, MonadReader)

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Servant.Auth.Server (Auth, AuthResult
          , JWT, JWTSettings, FromJWT, ToJWT
          , BasicAuth, BasicAuthData, BasicAuthCfg, FromBasicAuthData (..)
        )
import Servant.Server
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic ((:-), ToServantApi, ToServant)
import Servant.API ((:>), ReqBody, JSON, Capture, Post, Get)

import Hasql.Pool (Pool)
import Database.MySQL.Base (MySQLConn)

import Foreign (Ptr)

import qualified Options.Runtime as Rt


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


data LoginRequest =
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
newtype Html = Html { html :: ByteString }

-- Context to all API handlers (ReaderT)

data AppEnv = AppEnv {
    config_Ctxt :: Rt.RunOptions
    , jwt_Ctxt :: JWTSettings
    , pgPool_Ctxt :: Pool
    , mqlConn_Ctxt :: MySQLConn
    , sapiModuleDef_Ctxt :: Ptr ()
  }


-- Error management:
data ApiError =
  DoesNotExistAE Text
  | InternalErrorAE Text
  | UnauthorizedAE
  | UnknownSituationAE Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


asServantError :: ApiError -> ServerError
asServantError error =
  case error of
    DoesNotExistAE  msg-> err404 { errBody = LBS.fromStrict . encodeUtf8 $ msg }
    InternalErrorAE msg -> err500 { errBody = LBS.fromStrict . encodeUtf8 $ msg }
    UnauthorizedAE -> err401 { errBody = "Unauthorized access." }
    UnknownSituationAE msg -> err500 { errBody = LBS.fromStrict . encodeUtf8 $ msg }


-- Main Application / handler for the API:
newtype EasyVerseApp api = EasyVerseApp {
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
    )


data AuthenticatedRoutes route = AuthenticatedRoutes {
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


data GetPageRoutes route = GetPageRoutes { 
    getPage :: route :- "p" :> Capture "path" String :> Get '[HTML] Html
  }
  deriving stock (Generic)

