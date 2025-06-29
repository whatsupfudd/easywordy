{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.Apps.Scenario.Presentation.DbContainers where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Options, toJSON, defaultOptions, genericToJSON, fieldLabelModifier)


cutFieldP3 :: Options
cutFieldP3 = defaultOptions { fieldLabelModifier = init . init . init }


data TopLevelPrez = TopLevelPrez {
  uidTLP :: Int32
  , eidTLP :: UUID
  , labelTLP :: Text
  , notesTLP :: Text
  , createdAtTLP :: UTCTime
  -- , updatedAtTLP :: UTCTime
  }
  deriving (Show, Generic, FromJSON)


instance ToJSON TopLevelPrez where
  toJSON = genericToJSON cutFieldP3


data TopLevelAct = TopLevelAct {
      uidTLA :: Int32
    , labelTLA :: Text
    , seqNbrTLA :: Int32
    , createdAtTLA :: UTCTime
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON TopLevelAct where
  toJSON = genericToJSON cutFieldP3
