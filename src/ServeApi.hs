{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module ServeApi where

import Control.Monad.Except (ExceptT, MonadError, withExceptT)
import Control.Monad.Reader (runReaderT)

import Data.List.NonEmpty (NonEmpty (..))

import GHC.Generics

import Servant (Proxy (..), Context (..), hoistServerWithContext, serveWithContext, Handler (..))
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.Auth.Server (Auth, AuthResult (..), IsSecure (NotSecure)
            , BasicAuth, BasicAuthCfg, FromBasicAuthData
            , CookieSettings (CookieSettings, cookieIsSecure), defaultCookieSettings
            , JWT, JWTSettings, FromJWT (..), ToJWT (..), defaultJWTSettings )
import qualified Servant.Auth.Server as Sauth
import Servant.API ((:>), JSON)
import Servant.API.Generic ((:-), ToServantApi, ToServant)
import Servant.Multipart (defaultMultipartOptions, MultipartOptions (..), Tmp)

import Network.Wai (Application)
import Network.Wai.Parse (setMaxRequestKeyLength, defaultParseRequestBodyOptions)
import Network.Wai.Middleware.RequestLogger (logStdout)

import Database.MySQL.Base (MySQLConn)
import Hasql.Pool (Pool)

import Foreign (Ptr)

import HttpSup.JWT (readJWK, tmpJWK)
import HttpSup.CorsPolicy (setCorsPolicy)
import Api.Types
-- Old stuff:
import Api.Session (validateUser)
import Api.Handlers (anonHandlers, authHandlers)
-- New routing system:
import Routing.TopDef (TopRoutes)
import Routing.TopHandlers (serverApiT)
import WordPress.Wrapper (beginPhp)
import qualified Options.Runtime as Rt


-- Routing of requests (API definition):
import Wapp.Types (RoutingDictionary)

type MainApi = ToServantApi TopRoutes


-- serveApi ::  Rt.RunOptions -> Pool -> IO Application
launchServant ::  Rt.RunOptions -> Pool -> MySQLConn -> Ptr () -> RoutingDictionary -> IO Application
launchServant rtOpts pgPool mqlConn sapiModuleDef appDefs = do
  putStrLn $ "@[launchServant] starting, confFile: " <> show rtOpts.jwkConfFile <> "."
  -- TODO: figure out how to turn off JWT authentication.
  jwtKey <- maybe tmpJWK readJWK rtOpts.jwkConfFile
  putStrLn "@[serveApi] got jwt keys."

  let
    linkUp :: NonEmpty (a -> a) -> a -> a
    linkUp = foldr1 (.)

    cookieCfg = defaultCookieSettings { cookieIsSecure = NotSecure }
    jwtSettings = Sauth.defaultJWTSettings jwtKey
    -- sessionAuth = validateUser dbPool
    sessionAuth = validateUser

    -- For file upload support, will be used later:
    multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Tmp)) {
        generalOptions = setMaxRequestKeyLength 512 defaultParseRequestBodyOptions
      }

    apiContext = cookieCfg :. jwtSettings :. sessionAuth :. multipartOpts :. EmptyContext
    apiContextP = Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCfg]

    middlewares = case rtOpts.corsPolicy of
      Nothing -> linkUp $ id :| [ logStdout ]
      Just aPolicy -> linkUp $ id :| [ logStdout, setCorsPolicy aPolicy ]

    -- TODO: add errorMw @JSON @'["message", "status"] when Servant.Errors is compatible with aeson-2.
    -- TODO: enable corsMiddleware based on rtOpts.
    -- appEnv = AppEnv { config_Ctxt = rtOpts, jwt_Ctxt = jwtSettings, dbPool_Ctxt = dbPool }
    appEnv = AppEnv { config_Ctxt = rtOpts, jwt_Ctxt = jwtSettings, pgPool_Ctxt = pgPool, mqlConn_Ctxt = mqlConn
            , sapiModuleDef_Ctxt = sapiModuleDef, routeDict_Ctxt = appDefs }
    server = hoistServerWithContext serverApiProxy apiContextP (apiAsHandler appEnv) serverApiT

  putStrLn $ "@[serveApi] listening on port " <> show rtOpts.serverPort <> "."
  beginPhp sapiModuleDef
  pure $ middlewares $ serveWithContext serverApiProxy apiContext server


-- Root handling definition:

serverApiProxy :: Proxy MainApi
serverApiProxy = Proxy

-- | Natural transformations between 'App' and 'Handler' monads
apiAsHandler :: AppEnv -> EasyVerseApp a -> Handler a
apiAsHandler e =
  Handler . withExceptT asServantError . flip runReaderT e . apiHandler
