{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
module Wapp.State where

import Control.Concurrent.STM (TVar, TMVar)

import Data.Text (Text)
import qualified Data.Map as Mp
import Data.UUID (UUID)

import Wapp.AppDef (ResolvedApp, RoutingDictionary)
import Wapp.FileWatcher (WatcherControl)


-- Overall state of execution of the Wapp side of EasyWordy:
data LiveWapp = LiveWapp {
  wapp :: ResolvedApp
  , watcher :: Maybe WatcherControl
  , signaler :: TMVar ()
  , commChannel :: TVar FilePath
  }


instance Show LiveWapp where
  show liveApp = "LiveWapp { wapp = " <> show liveApp.wapp <> ", watcher = " <> show liveApp.watcher <> " }"


data Session = Session {
  idSE :: UUID
  , userSE :: User
  }
 deriving (Show)


data User = User {
  userID :: UUID
  , profile :: UserProfile
  }
 deriving (Show)


data UserProfile = UserProfile {
  name :: Text
  , email :: Text
  , avatar :: Maybe Text
  , prefLocale :: Text
  }
 deriving (Show)


fakeSession :: UUID -> UUID -> Session
fakeSession fakeSessionID fakeUserID = Session {
    idSE = fakeSessionID
    , userSE = User {
      userID = fakeUserID
      , profile = UserProfile {
        name = "fakeUser"
        , email = "fakeUser@example.com"
        , avatar = Nothing
        , prefLocale = "en"
        }
      }
  }


data WappState = WappState {
  appDefs :: RoutingDictionary
  , cache :: Mp.Map UUID LiveWapp
  , sessions :: Mp.Map UUID Session
  }
 deriving (Show)


newtype ErrMessage = ErrMessage Text
  deriving (Show, Eq, Ord)

