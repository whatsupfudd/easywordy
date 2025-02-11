{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Demo.RouteDef where

import Data.Text (Text)
import GHC.Generics (Generic)

import Servant.API ((:>), Get, ReqBody, Post)
import Servant.API.Generic ((:-))
import Servant.API.ContentTypes (FormUrlEncoded)
import Web.FormUrlEncoded (FromForm (..))

import Api.Types (HTML, Html)


newtype SearchContent = SearchContent {
    search :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm)


data DemoTopRoutes mode = DemoTopRoutes {
  demoWs :: mode :- "demo-ws" :> Get '[HTML] Html
  , demoSrch :: mode :- "xsearch" :> ReqBody '[FormUrlEncoded] SearchContent :> Post '[HTML] Html
 }
 deriving stock (Generic)