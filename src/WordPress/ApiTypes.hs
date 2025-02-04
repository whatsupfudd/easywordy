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

import qualified Network.WebSockets as WS

import Servant.API ((:>), Capture, Capture', CaptureAll, Post, Get, QueryParam', QueryString, ReqBody, FormUrlEncoded, NamedRoutes)
import Servant.API.QueryParam (QueryParam')
import Servant.API.Modifiers (Optional, Strict, Lenient)
import Servant.API.Generic ((:-), ToServantApi)
import Web.FormUrlEncoded (FromForm (..), parseUnique)
import Servant.API.WebSocket (WebSocket)

import Data.Aeson (FromJSON)

import Api.Types (HTML, Html)


newtype LangBody = LangBody {
    language :: Text
  }
  deriving (Show, Generic)

newtype IndexPosting =
  Language LangBody
  deriving (Show, Generic)

instance FromForm IndexPosting where
  fromForm f = Language . LangBody <$> parseUnique "language" f
  
data Trackback = Trackback {
  nameB :: Text
  , urlB :: Text
  , excerptB :: Text
  , blogLabel :: Text
  , permaLinkB :: Text
  , titleB :: Text
  , body :: Text
  , author :: Text
  , authorEmail :: Text
  }
  deriving (Show, Generic, FromForm)

data PostComment = PostComment {
  comment :: Text
  , author :: Text
  , authorEmail :: Text
  , authorUrl :: Text
  , content :: Text
  , postId :: Int
  , pcType :: Text
  }
  deriving (Show, Generic, FromForm)

data InstallPost = InstallPost {
    language :: Maybe Text
  }
  deriving (Show, Generic, FromForm)

newtype SearchContent = SearchContent {
    search :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm)


data TopRoutesWP mode = TopRoutesWP {
    -- CMS (content and operations):
      admin :: mode :- "wp-admin" :> ToServantApi AdminRoutesWP
    -- Includes:
    , includes :: mode :- "wp-includes" :> ToServantApi IncludesRoutes
    -- Basic content delivery:
    , homePage :: mode :- QueryParam' '[Optional, Lenient] "p" Int :> Get '[HTML] Html
    , indexGet :: mode :- "index.php" :> QueryParam' '[Optional, Lenient] "p" Int
           :> QueryParam' '[Optional, Lenient] "page_id" Int
           :> QueryParam' '[Optional, Lenient] "cat" Int
           :> QueryParam' '[Optional, Lenient] "tag" Text
           :> QueryParam' '[Optional, Lenient] "s" Text
           :> QueryParam' '[Optional, Lenient] "m" Text
           :> QueryParam' '[Optional, Lenient] "author" Int
           :> QueryParam' '[Optional, Lenient] "name" Text
           :> Get '[HTML] Html
    , indexPost :: mode :- "index.php" :> QueryParam' '[Optional, Lenient] "step" Int
            :> ReqBody '[FormUrlEncoded] IndexPosting :> Post '[HTML] Html
    -- User management:
    , login :: mode :- "wp-login.php" :> Get '[HTML] Html
    , signup :: mode :- "wp-signup.php" :> Get '[HTML] Html
    -- XML API endpoints:
    , xmlRpc :: mode :- "xmlrpc.php" :> Get '[HTML] Html
    -- Posting:
    , trackback :: mode :- "wp-trackback.php" :> ReqBody '[FormUrlEncoded] Trackback :> Post '[HTML] Html
    -- External bookkeeping:
    , cron :: mode :- "wp-cron.php" :> QueryParam' '[Optional, Lenient] "doing_wp_cron" Text :> Post '[HTML] Html
    -- EasyWordy own stuff:
    , easywordy :: mode :- "wbap" :> ToServantApi EasyWordyRoutes
  }
  deriving stock (Generic)


data AdminRoutesWP mode = AdminRoutesWP {
      -- admin basics (dashboard, options, etc):
      root :: mode :- Get '[HTML] Html
      , index :: mode :- "index.php" :> Get '[HTML] Html
      -- Post management:
      , postNew :: mode :- "post-new.php" :> Get '[HTML] Html
      , post :: mode :- "post.php" :> Capture "id" Int :> Get '[HTML] Html
      , postPages :: mode :- "edit.php"
              :> QueryParam' '[Optional, Lenient] "post_type" Text
              :> Get '[HTML] Html
      , postTags :: mode :- "edit-tags.php" :> Capture "taxonomy" Text :> Capture "id" Int :> Get '[HTML] Html
      , upload :: mode :- "upload.php" :> Get '[HTML] Html
      , mediaNew :: mode :- "media-new.php" :> Get '[HTML] Html
      -- Comments:
      , postComment :: mode :- "wp-comments-post.php" :> ReqBody '[FormUrlEncoded] PostComment :> Post '[HTML] Html
      -- Appearance, themes, widgets, menus:
      , themes :: mode :- "themes.php" :> Get '[HTML] Html
      , siteEditor :: mode :- "site-editor.php" :> Capture "path" Text :> Get '[HTML] Html
      , customize :: mode :- "customize.php" :> Capture "return" Text :> Get '[HTML] Html
      , widgets :: mode :- "widgets.php" :> Get '[HTML] Html
      , themesInstall :: mode :- "theme-install.php" :> Get '[HTML] Html
      , menus :: mode :- "nav-menus.php" :> Capture "action" Text :> Capture "menu" Int :> Get '[HTML] Html
      -- Plugins management:
      , plugins :: mode :- "plugins.php" :> Get '[HTML] Html
      , pluginsInstall :: mode :- "plugin-install.php" :> Get '[HTML] Html
      -- Users:
      , users :: mode :- "users.php" :> Get '[HTML] Html
      , usersNew :: mode :- "user-new.php" :> Get '[HTML] Html
      , profile :: mode :- "profile.php" :> Get '[HTML] Html
      -- Tools:
      , tools :: mode :- "tools.php" :> Get '[HTML] Html
      , importExport :: mode :- "import.php" :> Get '[HTML] Html
      , export :: mode :- "export.php" :> Get '[HTML] Html
      , exportPersonalData :: mode :- "export-personal-data.php" :> Get '[HTML] Html
      , erasePersonalData :: mode :- "erase-personal-data.php" :> Get '[HTML] Html
      -- Settings:
      , settings :: mode :- "settings.php" :> Get '[HTML] Html
      , optionsGeneral :: mode :- "options-general.php" :> Get '[HTML] Html
      , optionsWriting :: mode :- "options-writing.php" :> Get '[HTML] Html
      , optionsReading :: mode :- "options-reading.php" :> Get '[HTML] Html
      , optionsDiscussion :: mode :- "options-discussion.php" :> Get '[HTML] Html
      , optionsMedia :: mode :- "options-media.php" :> Get '[HTML] Html
      , optionsPermalinks :: mode :- "options-permalink.php" :> Get '[HTML] Html
      , optionsPrivacy :: mode :- "options-privacy.php"
            :> QueryParam' '[Optional, Lenient] "tab" Text
            :> Get '[HTML] Html
      -- Ajax API endpoints:
      , ajaxAdmin :: mode :- "admin-ajax.php" :> Get '[HTML] Html
      , commentAdmin :: mode :- "comment.php" :> Capture "id" Int :> Get '[HTML] Html
      -- Initial install logic:
      , installGet :: mode :- "install.php" :> Get '[HTML] Html
      , installPost :: mode :- "install.php" :> QueryParam' '[Optional, Lenient] "step" Int :> ReqBody '[FormUrlEncoded] InstallPost :> Post '[HTML] Html

  }
  deriving stock (Generic)


data EasyWordyRoutes mode = EasyWordyRoutes {
    indexZN :: mode :- "index.php" :> QueryParam' '[Optional, Lenient] "p" Int :> Get '[HTML] Html
    , demoWs :: mode :- "demo-ws" :> Get '[HTML] Html
    , demoSrch :: mode :- "xsearch" :> ReqBody '[FormUrlEncoded] SearchContent :> Post '[HTML] Html
    , xStatic :: mode :- "xstatic" :> CaptureAll "path" String :> Get '[HTML] Html
    -- TODO: find out how to make the capture optional, giving a Maybe Text.
    , wsStream :: mode :- "stream" :> Capture "sid" Text :> WebSocket
    , rootZN :: mode :- Get '[HTML] Html
  }
  deriving stock (Generic)


data IncludesRoutes mode = IncludesRoutes {
    catchAll :: mode :- CaptureAll "subpath" String :> QueryString :> Get '[HTML] Html
  }
  deriving stock (Generic)
