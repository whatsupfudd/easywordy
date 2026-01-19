{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Wapp.ApiTypes where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics

import Servant.API
  ( (:>)
  , Capture
  , CaptureAll
  )

import Data.Aeson (FromJSON, ToJSON)


-- ============================================================================
-- Payload types (v1)
-- ============================================================================

data LoginReq = LoginReq
  { ident :: Text
  , password :: Text
  , wappId :: Maybe UUID
  , resumeContext :: Maybe UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LoginReply = LoginReply
  { sid :: Text
  , cid :: UUID
  , userId :: UUID
  , displayName :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype LogoutReq = LogoutReq
  { sid :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RenewReq = RenewReq
  { sid :: Text
  , cid :: UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RenewReply = RenewReply
  { sid :: Text
  , cid :: UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UserSelf = UserSelf
  { userId :: UUID
  , primaryEmail :: Maybe Text
  , displayName :: Maybe Text
  , avatarUrl :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UpdateUserSelfReq = UpdateUserSelfReq
  { displayName :: Maybe Text
  , avatarUrl :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SessionInfo = SessionInfo
  { sid :: Text
  , cid :: UUID
  , createdAt :: Text
  , lastSeenAt :: Text
  , userAgent :: Maybe Text
  , ipAddr :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PermissionSet = PermissionSet
  { wappId :: Maybe UUID
  , roles :: [Text]
  , permissions :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AdminUserSummary = AdminUserSummary
  { userId :: UUID
  , primaryEmail :: Maybe Text
  , displayName :: Maybe Text
  , status :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AdminCreateUserReq = AdminCreateUserReq
  { email :: Text
  , displayName :: Maybe Text
  , initialPassword :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AdminSetUserStatusReq = AdminSetUserStatusReq
  { status :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WappMemberSummary = WappMemberSummary
  { userId :: UUID
  , role :: Text
  , addedAt :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AddWappMemberReq = AddWappMemberReq
  { userId :: UUID
  , role :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype SetWappMemberRoleReq = SetWappMemberRoleReq
  { role :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
