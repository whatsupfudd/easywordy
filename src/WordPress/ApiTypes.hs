{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module WordPress.ApiTypes where

import Data.Text (Text)
import GHC.Generics

import Servant.API.Generic ((:-))
import Servant.API ((:>), Capture, Post, Get, QueryParam', ReqBody, FormUrlEncoded)
import Servant.API.QueryParam (QueryParam')
import Servant.API.Modifiers (Optional, Strict, Lenient)
import Web.FormUrlEncoded (FromForm (..), parseUnique)


import Api.Types (HTML, Html)


data LangBody = LangBody {
    language :: Text
  }
  deriving (Show, Generic)

data IndexPosting =
  Language LangBody
  deriving (Show, Generic)

instance FromForm IndexPosting where
  fromForm f = Language . LangBody <$> parseUnique "language" f
  

data WordPressRoutes route = WordPressRoutes {
    login :: route :- "wp-login.php" :> Get '[HTML] Html
    , admin :: route :- "wp-admin" :> Capture "path" String :> Get '[HTML] Html
    , cron :: route :- "wp-cron.php" :> Get '[HTML] Html
    , indexGet :: route :- "index.php" :> QueryParam' '[Optional, Lenient] "p" Int
           :> QueryParam' '[Optional, Lenient] "page_id" Int
           :> QueryParam' '[Optional, Lenient] "cat" Int
           :> QueryParam' '[Optional, Lenient] "tag" Text
           :> QueryParam' '[Optional, Lenient] "s" Text
           :> QueryParam' '[Optional, Lenient] "m" Text
           :> QueryParam' '[Optional, Lenient] "author" Int
           :> QueryParam' '[Optional, Lenient] "name" Text
           :> Get '[HTML] Html
    , indexPost :: route :- "index.php" :> QueryParam' '[Optional, Lenient] "step" Int
            :> ReqBody '[FormUrlEncoded] IndexPosting :> Post '[HTML] Html
    , homePage :: route :- Get '[HTML] Html
  }
  deriving stock (Generic)
