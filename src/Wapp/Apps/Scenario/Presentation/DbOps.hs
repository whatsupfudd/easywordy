{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.Apps.Scenario.Presentation.DbOps where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
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

import Data.Aeson (encode, FromJSON (..), ToJSON, fromJSON, Result(..), withObject, (.:))

import qualified Wapp.Apps.Scenario.Presentation.DbContainers as Dc
import Wapp.AppDef (NativeLibFunction)
 -- Pool -> IO (Either String (Vector Dc.TopLevelPrez))

getTopLevelPrez :: NativeLibFunction
getTopLevelPrez dbPool _ = do
  ieRezA <- use dbPool $ do
    H.statement (0, 10) [H.vectorStatement|
        select
          uid::int4, eid::uuid, pname::text, notes::text, created_at::timestamptz
        from presentations
        order by created_at desc
        offset $1::int4 limit $2::int4
      |]
  case ieRezA of
    Left err -> pure . Left . show $ err
    Right prez -> pure . Right . encode $ V.map dbToTopLevelPrez prez
  where
  dbToTopLevelPrez :: (Int32, UUID, Text, Text, UTCTime) -> Dc.TopLevelPrez
  dbToTopLevelPrez (uid, eid, label, notes, createdAt) = Dc.TopLevelPrez uid eid label notes createdAt


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
            acts a
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
                acts a
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

newtype ScenarioTree = ScenarioTree {
    actsST :: Vector ActV1
  }
  deriving (Show, Generic, ToJSON)

data ActV1 = ActV1 {
    eidAC :: UUID
    , createdAtAC :: UTCTime
    , seqNbrAC :: Int32
    , scenesAC :: Vector SceneV1
  }
  deriving (Show, Generic, ToJSON)

data SceneV1 = SceneV1 {
    eidSC :: UUID
    , createdAtSC :: UTCTime
    , seqNbrSC :: Int32
    , shotsSC :: Vector ShotV1
  }
  deriving (Show, Generic, ToJSON)


data ShotV1 = ShotV1 {
    uidSV :: Int32
    , createdAtSV :: UTCTime
    , seqNbrSV :: Int32
    , dialoguesSV :: Vector DialogueV1
  }
  deriving (Show, Generic, ToJSON)


data DialogueV1 = DialogueV1 {
    uidDV :: Int32
    , actorDV :: Text
    , lineDV :: Text
    , emotionDV :: Text
    , dResourcesDV :: Vector DResourceV1
  }
  deriving (Show, Generic, ToJSON)

data DResourceV1 = DResourceV1 {
    uidDR :: Int32
    , seqNbrDR :: Int32
    , kindDR :: Text
    , contentDR :: Text
    , createdAtDR :: UTCTime
  }
  deriving (Show, Generic, ToJSON)

type RawOutType = (
      Int32
    , Int32, UUID, UTCTime
    , Int32, UUID, UTCTime
    , Int32, Int32, Text, UTCTime
    , Int32, Text, Text, Maybe Text
    , Int32, Int32, Text, Text, UTCTime
    )
type ActMap = Mp.Map UUID (UUID, UTCTime, Int32, SceneMap)
type SceneMap = Mp.Map UUID (UUID, UTCTime, Int32, ShotMap)
type ShotMap = Mp.Map Int32 (Int32, UTCTime, Int32, DialogueMap)
type DialogueMap = Mp.Map Int32 (Int32, Text, Text, Text, Vector DResourceV1)

newtype PrezEid = PrezEid {
  eidPE :: UUID
}
  deriving (Show, Generic)

instance FromJSON PrezEid where
  parseJSON = withObject "PrezEid" $ \v -> PrezEid <$> v .: "eid"


getFullPrezTree :: NativeLibFunction
getFullPrezTree dbPool (value, mbLabel) = do
  case fromJSON value :: Result PrezEid of
    Error err -> pure . Left $ "@[getFullPrezTree] can't decode Value: " <> err
    Success args -> do
      putStrLn $ "@[getFullPrezTree] args: " <> show args
      ieRawTrees <- use dbPool $ do
        H.statement args.eidPE [H.vectorStatement|
          select
            b.uid::int4
            , c.sequence_number::int4, c.eid::uuid, c.created_at::timestamptz
            , d.sequence_number::int4, d.eid::uuid, d.created_at::timestamptz
            , e.sequence_number::int4, e.uid::int4, e.kind::text, e.created_at::timestamptz
            , f.uid::int4, f.actor::text, f.dtext::text, f.influence::text?
            , g.uid::int4, g.seqnbr::int4, g.kind::text, g.content::text, g.created_at::timestamptz
          from
            presentations as a
            join scenarios as b on b.prez_fk = a.uid
            join acts as c on c.scenario_fk = b.uid
            join scenes as d on d.act_fk = c.uid
            join actions as e on e.scene_fk = d.uid 
            join dialogues as f on f.action_fk = e.uid
            join dialogueresources as g on g.dialogue_fk = f.uid
          where a.eid = $1::uuid
          order by
            b.created_at
            , c.sequence_number
            , d.sequence_number
            , e.sequence_number
        |]
      case ieRawTrees of
        Left err -> pure . Left . show $ err
        Right scenarioTrees -> pure . Right . encode $ rawTreeToScenarioTree scenarioTrees

-- rawValues = (actSqNbr, actEid, actCreatedAt, sceneSqNbr, sceneEid, sceneCreatedAt, shotSqNbr, shotKind, shotCreatedAt, diaActor, diaLine, diaEmotion)

rawTreeToScenarioTree :: V.Vector RawOutType -> ScenarioTree
rawTreeToScenarioTree rawRows =
  let
    actMap = foldl (
      \accum rawRow@(scenarioUid
            , actSqNbr, actEid, actCreatedAt
            , sceneSqNbr, sceneEid, sceneCreatedAt
            , shotSqNbr, shotUid, shotKind, shotCreatedAt
            , diaUid, diaActor, diaLine, diaEmotion
            , dResUid, dResSeqNbr, dResKind, dResContent, dResCreatedAt
            ) ->
        case Mp.lookup actEid accum of
          Nothing -> Mp.insert actEid (buildNewAct rawRow) accum
          Just (actEid, actCreatedAt, actSqNbr, actSceneMap) ->
            case Mp.lookup sceneEid actSceneMap of
              Nothing ->
                let
                  updActTree = (actEid, actCreatedAt, actSqNbr, Mp.insert sceneEid (buildNewScene rawRow) actSceneMap)
                in
                  Mp.insert actEid updActTree accum
              Just (sceneEid, sceneCreatedAt, sceneSqNbr, sceneShotMap) ->
                case Mp.lookup shotUid sceneShotMap of
                  Nothing ->
                    let
                      updSceneTree = (sceneEid, sceneCreatedAt, sceneSqNbr, Mp.insert shotUid (buildNewShot rawRow) sceneShotMap)
                      updActTree = (actEid, actCreatedAt, actSqNbr, Mp.insert sceneEid updSceneTree actSceneMap)
                    in
                      Mp.insert actEid updActTree accum
                  Just (shotUid, shotCreatedAt, shotSqNbr, shotDialogueMap) ->
                    case Mp.lookup diaUid shotDialogueMap of
                      Nothing ->
                        let
                          updShotTree = (shotUid, shotCreatedAt, shotSqNbr, Mp.insert diaUid (buildNewDialogue rawRow) shotDialogueMap)
                          updSceneTree = (sceneEid, sceneCreatedAt, sceneSqNbr, Mp.insert shotUid updShotTree sceneShotMap)
                          updActTree = (actEid, actCreatedAt, actSqNbr, Mp.insert sceneEid updSceneTree actSceneMap)
                        in
                          Mp.insert actEid updActTree accum
                      Just (diaUid, diaActor, diaLine, diaEmotion, diaDResources) ->
                        let
                          nDRes = buildNewDResource rawRow
                          updDialogueTree = (diaUid, diaActor, diaLine, diaEmotion, V.snoc diaDResources nDRes)
                          updShotTree = (shotUid, shotCreatedAt, shotSqNbr, Mp.insert diaUid updDialogueTree shotDialogueMap)
                          updSceneTree = (sceneEid, sceneCreatedAt, sceneSqNbr, Mp.insert shotUid updShotTree sceneShotMap)
                          updActTree = (actEid, actCreatedAt, actSqNbr, Mp.insert sceneEid updSceneTree actSceneMap)
                        in
                          Mp.insert actEid updActTree accum
      ) Mp.empty rawRows
  in
  ScenarioTree (mapToV1Items actMap)


mapToV1Items :: ActMap -> Vector ActV1
mapToV1Items actMap = 
  V.fromList $ map (\(actEid, (_, actCreatedAt, actSqNbr, sceneMap)) -> 
    ActV1 actEid actCreatedAt actSqNbr (sceneMapToV1Items sceneMap)
  ) (Mp.toList actMap)


sceneMapToV1Items :: SceneMap -> Vector SceneV1
sceneMapToV1Items sceneMap = 
  V.fromList $ map (\(sceneEid, (_, sceneCreatedAt, sceneSqNbr, shotMap)) -> 
    SceneV1 sceneEid sceneCreatedAt sceneSqNbr (shotMapToV1Items shotMap)
  ) (Mp.toList sceneMap)


shotMapToV1Items :: ShotMap -> Vector ShotV1
shotMapToV1Items shotMap = 
  V.fromList $ map (\(shotUid, (_, shotCreatedAt, shotSqNbr, dialogues)) -> 
    ShotV1 shotUid shotCreatedAt shotSqNbr (dialogueMapToV1Items dialogues)
  ) (Mp.toList shotMap)

dialogueMapToV1Items :: DialogueMap -> Vector DialogueV1
dialogueMapToV1Items dialogueMap = 
  V.fromList $ map (\(diaUid, (_, diaActor, diaLine, diaEmotion, dResources)) -> 
    DialogueV1 diaUid diaActor diaLine diaEmotion dResources
  ) (Mp.toList dialogueMap)



buildNewAct :: RawOutType -> (UUID, UTCTime, Int32, SceneMap)
buildNewAct rawRow@(scenarioUid
        , actSqNbr, actEid, actCreatedAt
        , sceneSqNbr, sceneEid, sceneCreatedAt, shotSqNbr, shotUid, shotKind, shotCreatedAt, diaUid, diaActor, diaLine, diaEmotion
        , dResUid, dResSeqNbr, dResKind, dResContent, dResCreatedAt) =
  let
    nScene = buildNewScene rawRow
  in
  (actEid, actCreatedAt, actSqNbr, Mp.singleton sceneEid nScene)


buildNewScene :: RawOutType -> (UUID, UTCTime, Int32, ShotMap)
buildNewScene rawRow@(scenarioUid
        , actSqNbr, actEid, actCreatedAt
        , sceneSqNbr, sceneEid, sceneCreatedAt, shotSqNbr, shotUid, shotKind, shotCreatedAt
        , diaUid, diaActor, diaLine, diaEmotion
        , dResUid, dResSeqNbr, dResKind, dResContent, dResCreatedAt) =
  let
    nShot = buildNewShot rawRow
  in
  (sceneEid, sceneCreatedAt, sceneSqNbr, Mp.singleton shotUid nShot)


buildNewShot :: RawOutType -> (Int32, UTCTime, Int32, DialogueMap)
buildNewShot rawRow@(scenarioUid
        , actSqNbr, actEid, actCreatedAt
        , sceneSqNbr, sceneEid, sceneCreatedAt, shotSqNbr, shotUid, shotKind, shotCreatedAt
        , diaUid, diaActor, diaLine, diaEmotion
        , dResUid, dResSeqNbr, dResKind, dResContent, dResCreatedAt) =
  let
    nDialogue = buildNewDialogue rawRow
  in
  (shotUid, shotCreatedAt, shotSqNbr, Mp.singleton diaUid nDialogue)


buildNewDialogue :: RawOutType -> (Int32, Text, Text, Text, Vector DResourceV1)
buildNewDialogue rawRow@(scenarioUid
        , actSqNbr, actEid, actCreatedAt
        , sceneSqNbr, sceneEid, sceneCreatedAt, shotSqNbr, shotUid, shotKind, shotCreatedAt
        , diaUid, diaActor, diaLine, diaEmotion
        , dResUid, dResSeqNbr, dResKind, dResContent, dResCreatedAt) =
  let
    nDRes = buildNewDResource rawRow
  in
  (diaUid, diaActor, diaLine, fromMaybe "" diaEmotion, V.singleton nDRes)

buildNewDResource :: RawOutType -> DResourceV1
buildNewDResource rawRow@(scenarioUid
        , actSqNbr, actEid, actCreatedAt
        , sceneSqNbr, sceneEid, sceneCreatedAt, shotSqNbr, shotUid, shotKind, shotCreatedAt
        , diaUid, diaActor, diaLine, diaEmotion
        , dResUid, dResSeqNbr, dResKind, dResContent, dResCreatedAt) =
  DResourceV1 dResUid dResSeqNbr dResKind dResContent dResCreatedAt