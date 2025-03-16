{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.Apps.Scenario.Presentation.DbContainers where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data TopLevelPrez = TopLevelPrez {
  uidTLP :: Int32
  , eidTLP :: UUID
  , labelTLP :: Text
  , notesTLP :: Text
  , createdAtTLP :: UTCTime
  , updatedAtTLP :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)


data TopLevelAct = TopLevelAct {
      uidTLA :: Int32
    , seqNbrTLA :: Int32
    , createdAtTLA :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)
