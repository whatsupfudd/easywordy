{-# LANGUAGE DeriveGeneric #-}

module Wapp.Apps.KnowDocs.DbContainers where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), ToJSON, Options, toJSON, defaultOptions, genericToJSON, fieldLabelModifier, (.:), Value (..))


cutFieldP2 :: Options
cutFieldP2 = defaultOptions { fieldLabelModifier = init . init }


data KnowDoc = KnowDoc {
  idKD :: Int32
  , eidKD :: UUID
  , titleKD :: Text
  , localeKD :: Text
  , createdAtKD :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON KnowDoc where
  toJSON = genericToJSON cutFieldP2


data BlockNode = BlockNode {
  depthBN :: Int32
  , blockIdBN :: Int32
  , parentBlockIdBN :: Maybe Int32
  , contentBN :: Maybe Text
  , seqPosBN :: Double
  , hasMoreBN :: Bool
  }
  deriving (Show, Generic)

instance ToJSON BlockNode where
  toJSON = genericToJSON cutFieldP2


data NewDocInfo = NewDocInfo {
  title :: Text
  , locale :: Text
  }
  deriving (Show, Generic)

instance FromJSON NewDocInfo where
  parseJSON (Object obj) =
    NewDocInfo <$> obj .: "title" <*> obj .: "locale"


data NewDocResult = NewDocResult {
  eidND :: UUID
  , titleND :: Text
  , localeND :: Text
  , createdAtND :: UTCTime
}
  deriving (Show, Generic)

instance ToJSON NewDocResult where
  toJSON = genericToJSON cutFieldP2
