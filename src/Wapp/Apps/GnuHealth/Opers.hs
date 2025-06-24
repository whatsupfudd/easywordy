{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Wapp.Apps.GnuHealth.Opers where

import Data.Int (Int16, Int32, Int64)
import Data.Map (Map)
import qualified Data.Map as Mp
import Data.Text (Text, splitOn, stripPrefix, unpack)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vc

import GHC.Generics
import qualified Data.Aeson as Ae

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)

import qualified Hasql.TH as TH

import Wapp.Apps.GnuHealth.Types


data HsRow = HsRow {
  field_1 :: Text
  , field_2 :: Int32
}
  deriving (Show, Eq, Generic, Ae.FromJSON, Ae.ToJSON)


gnuhealth_conf_commands_fetch :: Pool -> TablePosition -> IO (Either String (Vector HsRow))
gnuhealth_conf_commands_fetch dbPool position = do
  rezA <- use dbPool testFetch
  case rezA of
    Left err ->
      pure . Left $ "@[getUserMailboxes] listMailboxes err: " <> show err
    Right aRow -> do
      -- putStrLn $ "@[getUserMailboxes] raw mboxes: " <> show values
      pure . Right $ Vc.map (uncurry HsRow) aRow



type FetchIn = (Int32, Int32)
type FetchRawOut = (Text, Int32)

testFetch :: Session (Vector FetchRawOut)
testFetch =
  statement () [TH.vectorStatement|
    select
      'Test'::text, 10::int4
  |]

