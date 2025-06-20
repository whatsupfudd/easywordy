{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Wapp.Apps.Aox.Types where

import Control.Applicative

import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID)

import GHC.Generics
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae

import Utils.Json (cutFieldP1, cutFieldP2)


-- | Entry point into the Aox mailboxes.
newtype UserInfo = UserInfo {
    nameUI :: Text
  }
  deriving (Show, Eq, Generic)

instance Ae.FromJSON UserInfo where
  parseJSON = Ae.genericParseJSON cutFieldP2
instance Ae.ToJSON UserInfo where
  toJSON = Ae.genericToJSON cutFieldP2
