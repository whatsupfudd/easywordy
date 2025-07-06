{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Wapp.HtmxSupport where

import qualified Data.HashMap.Strict as Hm
import qualified Data.Aeson.KeyMap as Km
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), (.:), (.:?), (.=))
import qualified Data.Aeson as Ae


data HxWsHeaders = HxWsHeaders {
    request :: Text
    , trigger :: Maybe Text
    , triggerName :: Maybe Text
    , target :: Maybe Text
    , currentURL :: Text
    , mid :: Maybe Text
    , params :: Maybe Value
  }
  deriving stock (Show, Generic)


instance FromJSON HxWsHeaders where
  parseJSON (Object obj) = HxWsHeaders <$>
    obj .: "HX-Request"
    <*> obj .:? "HX-Trigger"
    <*> obj .:? "HX-Trigger-Name"
    <*> obj .:? "HX-Target"
    <*> obj .: "HX-Current-URL"
    <*> obj .:? "mid"
    <*> obj .:? "params"

data HxWsMessage = HxWsMessage {
    wsMessage :: Maybe Text
    , headers :: HxWsHeaders
    , content :: Maybe Text
    , formFields :: Maybe Value
  }
  deriving (Show, Generic)


instance FromJSON HxWsMessage where
  parseJSON (Object obj) = do
    -- Parse known fields
    wsMsg <- obj .:? "hxid-1"
    hdrs <- obj .: "HEADERS"
    cont <- obj .:? "content"
    
    -- Extract remaining fields by removing known ones
    let
      knownKeys = ["hxid-1", "HEADERS", "content"]
      additionalFields = Km.filterWithKey (\k _ -> k `notElem` knownKeys) obj
      formFields = if Km.null additionalFields then
          Nothing
        else
          Just $ Object additionalFields
    
    return $ HxWsMessage wsMsg hdrs cont formFields
  
  parseJSON _ = fail "Expected JSON Object for HxWsMessage"
