module HttpSup.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (NominalDiffTime)


data Request = Request {
    method :: HttpMethod
    , uri :: String
    , queryArgs :: Map String String
    , reqHeaders :: [(String, String)]
    , reqBody :: Maybe ByteString
  }

data HttpMethod = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | TRACE | CONNECT


data Response = Response {
    statusCode :: Int
    , respHeaders :: [(String, String)]
    , respBody :: Either HttpError ByteString
    , duration :: NominalDiffTime
  }

data HttpError = HttpError {
    errorCode :: Int
    , errorMessage :: ByteString
  }

