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
    , target :: Text
    , currentURL :: Text
    , mid :: Maybe Text
    , args :: Maybe Text
  }
  deriving stock (Show, Generic)


instance FromJSON HxWsHeaders where
  parseJSON (Object obj) = HxWsHeaders <$>
    obj .: "HX-Request"
    <*> obj .:? "HX-Trigger"
    <*> obj .:? "HX-Trigger-Name"
    <*> obj .: "HX-Target"
    <*> obj .: "HX-Current-URL"
    <*> obj .:? "mid"
    <*> obj .:? "args"


data HxWsMessage = HxWsMessage {
    wsMessage :: Maybe Text
    , headers :: HxWsHeaders
  }
  deriving (Show, Generic)


instance FromJSON HxWsMessage where
  parseJSON (Object obj) = HxWsMessage <$>
    obj .:? "hxid-1"
    <*> obj .: "HEADERS"
