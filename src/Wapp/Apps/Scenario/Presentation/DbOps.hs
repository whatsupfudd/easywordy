{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.Apps.Scenario.Presentation.DbOps where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics (Generic)

import qualified Hasql.TH as H
import qualified Hasql.Session as H
import Hasql.Pool (Pool, use)

import Data.Aeson (encode, FromJSON, ToJSON, fromJSON, Result(..))

import qualified Wapp.Apps.Scenario.Presentation.DbContainers as Dc
import Wapp.AppDef (NativeLibFunction)
 -- Pool -> IO (Either String (Vector Dc.TopLevelPrez))

getTopLevelPrez :: NativeLibFunction
getTopLevelPrez dbPool _ = do
  ieRezA <- use dbPool $ do
    H.statement (0, 10) [H.vectorStatement|
        select
          uid::int4, eid::uuid, label::text, notes::text, created_at::timestamptz, updated_at::timestamptz
        from presentations
        order by created_at desc
        offset $1::int4 limit $2::int4
      |]
  case ieRezA of
    Left err -> pure . Left . show $ err
    Right prez -> pure . Right . encode $ V.map dbToTopLevelPrez prez
  where
  dbToTopLevelPrez :: (Int32, UUID, Text, Text, UTCTime, UTCTime) -> Dc.TopLevelPrez
  dbToTopLevelPrez (uid, eid, label, notes, createdAt, updatedAt) = Dc.TopLevelPrez uid eid label notes createdAt updatedAt


data ActsForPrez = ActsForPrez {
  eid :: UUID
  , limit :: Int32
  , offset :: Int32
  }
  deriving (Show, Generic, FromJSON)

data ActsResult = ActsResult {
  offset :: Int32
  , pagination :: (Int32, Int32)
  , acts :: Vector Dc.TopLevelAct
  }
  deriving (Show, Generic, ToJSON)

getActsForPrez :: NativeLibFunction
getActsForPrez dbPool (value, mbLabel) =
  case fromJSON value :: Result ActsForPrez of
    Error err -> pure . Left $ "@[getActsForPrez] can't decode Value: " <> err
    Success args -> do
      putStrLn $ "@[getActsForPrez] args: " <> show args
      ieNbrActs <- use dbPool $ do
        H.statement args.eid [H.singletonStatement|
          select
            count(*)::int4
          from
            scenes a
            join scenarios s on a.scenario_fk = s.uid
            join presentations p on s.prez_fk = p.uid
          where p.eid = $1::uuid
        |]
      case ieNbrActs of
        Left err -> pure . Left . show $ err
        Right nbrActs -> do
          ieRezA <- use dbPool $
            H.statement (args.eid, args.offset, args.limit) [H.vectorStatement|
              select
                a.uid::int4, a.sequence_number::int4, a.created_at::timestamptz
              from
                scenes a
                join scenarios s on a.scenario_fk = s.uid
                join presentations p on s.prez_fk = p.uid
              where
                p.eid = $1::uuid
              order by sequence_number
              offset $2::int4 limit $3::int4
            |]
          case ieRezA of
            Left err -> pure . Left . show $ err
            Right acts -> pure . Right . encode $ ActsResult nbrActs (0, 1) (V.map dbToTopLevelAct acts)
  where
  dbToTopLevelAct :: (Int32, Int32, UTCTime) -> Dc.TopLevelAct
  dbToTopLevelAct (uid, seqNbr, createdAt) = Dc.TopLevelAct uid "fake-act" seqNbr createdAt

{-
data ScenesForAct = ScenesForAct {
  eid :: UUID
  , limit :: Int32
  , offset :: Int32
  }
  deriving (Show, Generic, FromJSON)

getScenesForAct :: NativeLibFunction
getScenesForAct dbPool (value, mbLabel) =
  case fromJSON value :: Result ScenesForAct of
    Error err -> pure . Left $ "@[getScenesForAct] can't decode Value: " <> err
    Success args -> do
      putStrLn $ "@[getScenesForAct] args: " <> show args
      ieNbrScenes <- use dbPool $ do
        H.statement args.eid [H.singletonStatement|
          select
            count(*)::int4
          from
            scenes a
            join scenarios s on a.scenario_fk = s.uid
            join presentations p on s.prez_fk = p.uid
          where p.eid = $1::uuid
        |]
      case ieNbrScenes of
        Left err -> pure . Left . show $ err
-}