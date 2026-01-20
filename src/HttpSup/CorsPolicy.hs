{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HttpSup.CorsPolicy where

import qualified Data.ByteString as Bs
import Data.CaseInsensitive as CI
-- import Data.List (elem, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Network.HTTP.Types.Header (HeaderName, hOrigin, hAuthorization, hContentType)
import Network.Wai
import Network.Wai.Middleware.Cors
-- import Prelude


defaultCorsPolicy :: CORSConfig
data CORSConfig = CORSConfig {
   allowedOrigins :: [ Text ]
  , publicPrefixes :: [ Text ]
  , maxAge :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)


defaultCorsPolicy = CORSConfig {
   allowedOrigins = [ "http://localhost", "http://localhost:8885", "http://ledna:8205" ]
  , publicPrefixes = [ "login", "site", "/", "index.php", "wp-login.php", "wp-admin", "wbap/ui" ]
  , maxAge = Nothing
  }


setCorsPolicy :: CORSConfig -> Middleware
setCorsPolicy corsConfig = cors $ \request ->
  if isPublicApi request
  then Just $ simpleCorsResourcePolicy { 
    corsMethods = simpleMethods <> [ "OPTIONS" ]
    , corsRequestHeaders = simpleHeaders <> [ hAuthorization, hContentType ] <> hxRequestHeaders
    , corsExposedHeaders = Just hxResponseHeaders
    }
  else Just $ CorsResourcePolicy { 
    corsOrigins = matchHostOrigin request
    , corsMethods = simpleMethods <> [ "DELETE", "OPTIONS" ]
    , corsRequestHeaders = simpleHeaders <> [ hAuthorization, hContentType ] <> hxRequestHeaders
    , corsExposedHeaders = Just hxResponseHeaders
    , corsMaxAge = corsConfig.maxAge
    , corsVaryOrigin = True
    , corsRequireOrigin = True
    , corsIgnoreFailures = True
    }
    where
      matchHostOrigin :: Request -> Maybe ([Origin], Bool)
      matchHostOrigin request = Just . fromMaybe ([], False) $ do
        -- Maybe monad: CORS will be denied when there's any Nothing.
        -- _vhost <- requestHeaderHost request
        origin <- lookup hOrigin (requestHeaders request)
        if CI.mk origin `elem` ciAllowedOrigins
        then return ([origin], True)
        else Nothing

      isPublicApi :: Request -> Bool
      isPublicApi req =
        let
          p = DT.intercalate "/" (pathInfo req)
        in
        any (`DT.isPrefixOf` p) corsConfig.publicPrefixes

      ciAllowedOrigins = CI.mk . encodeUtf8 <$> corsConfig.allowedOrigins

      hxRequestHeaders :: [HeaderName]
      hxRequestHeaders = CI.mk
        <$> [
          "HX-Request"
        , "HX-Trigger"
        , "HX-Trigger-Name"
        , "HX-Target"
        , "HX-Current-URL"
        , "HX-Prompt"
        , "HX-Boosted"
        , "HX-History-Restore-Request"
        , "X-Requested-With"
        ]

      hxResponseHeaders :: [HeaderName]
      hxResponseHeaders = CI.mk
        <$> [
          "HX-Location"
        , "HX-Push-Url"
        , "HX-Redirect"
        , "HX-Refresh"
        , "HX-Replace-Url"
        , "HX-Reswap"
        , "HX-Retarget"
        , "HX-Trigger"
        , "HX-Trigger-After-Settle"
        , "HX-Trigger-After-Swap"
        ]