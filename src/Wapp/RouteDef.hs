{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Wapp.RouteDef where

import Data.Text (Text)
import Data.Map as Mp
import Data.UUID (UUID)
import GHC.Generics

import qualified Network.WebSockets as WS

import Servant.API (
        (:>), Capture, CaptureAll, Get, Put, Patch
        , Post, ReqBody, PlainText, JSON, PostNoContent, DeleteNoContent
    )
import Servant.API.QueryParam (QueryParam, QueryParam')
import Servant.API.Modifiers (Optional, Lenient, Required, Strict)
import Servant.API.Generic ((:-), ToServantApi, ToServant)
import Servant.API.WebSocket (WebSocket)
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.Multipart (MultipartForm, Tmp, MultipartData, Mem)

import Data.Aeson (FromJSON)

import Api.Types (HTML, Html, EasyVerseApp)
import Wapp.ApiTypes


newtype WappTopRoutes mode = WappTopRoutes {
    wapp :: mode :- "wbap" :> ToServantApi WappRoutes
 }
 deriving stock (Generic)

{-
data NewAssetInfo = NewAssetInfo {
    upTicket :: UUID
    , originalName :: FilePath
  }
  deriving (Show, Generic, FromJSON)
  deriving via (MultipartForm Tmp NewAssetInfo)

-- instance MultipartForm Tmp NewAssetInfo
-}


data WappRoutes mode = WappRoutes {
    phpTest :: mode :- "index.php" :> QueryParam' '[Optional, Lenient] "p" Int :> Get '[HTML] Html
    , xStatic :: mode :- "xstatic" :> CaptureAll "path" String :> Get '[HTML] Html
    -- TODO: find out how to make the capture optional, giving a Maybe Text.
    , wsStream :: mode :- "stream" :> QueryParam' '[Required, Strict] "sid" Text :> WebSocket
    , upload :: mode :- "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String
    , rootZN :: mode :- Get '[HTML] Html

    , auth :: mode :- "auth" :> ToServantApi WappAuthRoutes
    , user :: mode :- "user" :> ToServantApi WappUserRoutes
    , authz :: mode :- "authz" :> ToServantApi WappAuthzRoutes
    , admin :: mode :- "admin" :> ToServantApi WappAdminRoutes
  }
  deriving stock (Generic)

data WappAuthRoutes mode = WappAuthRoutes {
    -- SSR login page
    loginPg :: mode :- "login" :> Get '[HTML] Html

    -- REST login (creates a new EW session and attaches/creates a context)
    , login :: mode :- "login" :> ReqBody '[JSON] LoginReq :> Post '[JSON] LoginReply

    -- Logout (revoke session)
    , logout :: mode :- "logout" :> ReqBody '[JSON] LogoutReq :> PostNoContent

    -- Renew session while keeping / re-attaching to a context
    , renew :: mode :- "renew" :> ReqBody '[JSON] RenewReq :> Post '[JSON] RenewReply

    -- OIDC/OAuth broker start + callback (Google/Microsoft/Facebook/Clerk/WorkOS adapters)
    , oauthStart :: mode :- "oauth" :> Capture "provider" Text :> QueryParam "next" Text :> Get '[HTML] Html
    , oauthCallback :: mode :- "oauth" :> Capture "provider" Text
                    :> QueryParam "code" Text
                    :> QueryParam "state" Text
                    :> QueryParam "error" Text
                    :> Get '[HTML] Html
  }
  deriving stock (Generic)


data WappUserRoutes mode = WappUserRoutes {
    -- Account pages (SSR)
    accountPg :: mode :- "account" :> Get '[HTML] Html
    , securityPg :: mode :- "security" :> Get '[HTML] Html

    -- REST: current user profile
    , me :: mode :- "me" :> QueryParam "sid" Text :> Get '[JSON] UserSelf
    , updateMe :: mode :- "me" :> QueryParam "sid" Text :> ReqBody '[JSON] UpdateUserSelfReq :> Patch '[JSON] UserSelf

    -- REST: session inventory (multi-device)
    , sessions :: mode :- "sessions" :> QueryParam "sid" Text :> Get '[JSON] [SessionInfo]
    , revokeSession :: mode :- "sessions" :> Capture "sidToRevoke" Text :> QueryParam "sid" Text :> DeleteNoContent
  }
  deriving stock (Generic)


newtype WappAuthzRoutes mode = WappAuthzRoutes {
    -- Return effective roles/permissions for the current user (optionally scoped to a wapp)
    myPerms :: mode :- "me" :> QueryParam "sid" Text :> QueryParam "wappId" UUID :> Get '[JSON] PermissionSet
  }
  deriving stock (Generic)


data WappAdminRoutes mode = WappAdminRoutes {
    users :: mode :- "users" :> ToServantApi WappAdminUsersRoutes
    , wapps :: mode :- "wapps" :> ToServantApi WappAdminWappsRoutes
  }
  deriving stock (Generic)


data WappAdminUsersRoutes mode = WappAdminUsersRoutes {
    listUsers :: mode :- Get '[JSON] [AdminUserSummary]
    , createUser :: mode :- ReqBody '[JSON] AdminCreateUserReq :> Post '[JSON] AdminUserSummary
    , getUser :: mode :- Capture "userId" UUID :> Get '[JSON] AdminUserSummary
    , setUserStatus :: mode :- Capture "userId" UUID :> "status" :> ReqBody '[JSON] AdminSetUserStatusReq :> Put '[JSON] AdminUserSummary
  }
  deriving stock (Generic)


newtype WappAdminWappsRoutes mode = WappAdminWappsRoutes {
    members :: mode :- Capture "wappId" UUID :> "members" :> ToServantApi WappAdminWappMembersRoutes
  }
  deriving stock (Generic)


data WappAdminWappMembersRoutes mode = WappAdminWappMembersRoutes {
    listMembers :: mode :- Get '[JSON] [WappMemberSummary]
    , addMember :: mode :- ReqBody '[JSON] AddWappMemberReq :> Post '[JSON] WappMemberSummary
    , setMemberRole :: mode :- Capture "userId" UUID :> "role" :> ReqBody '[JSON] SetWappMemberRoleReq :> Put '[JSON] WappMemberSummary
    , removeMember :: mode :- Capture "userId" UUID :> DeleteNoContent
  }
  deriving stock (Generic)