{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Wapp.HtmxSupport where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON (..), Value (Object), (.:), (.:?), (.=))


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
  }
  deriving (Show, Generic)


instance FromJSON HxWsMessage where
  parseJSON (Object obj) = HxWsMessage <$>
    obj .:? "hxid-1"
    <*> obj .: "HEADERS"
    <*> obj .:? "content"
