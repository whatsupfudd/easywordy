{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Wapp.Apps.Scenario.Presentation.Storage where

import Data.Profunctor (dimap)
import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)

import GHC.Generics (Generic)
import qualified Data.Aeson as Ae

import Hasql.Session (Session, ResultError, statement, sql)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH
import qualified Hasql.Encoders as He
import qualified Hasql.Decoders as Hd

import Wapp.Apps.Scenario.Presentation.Types
import Wapp.AppDef (NativeLibFunction)
import Data.UUID (UUID)

storePresentation :: Pool -> Presentation -> IO (Either String ())
storePresentation dbPool prez = do
  use dbPool (sql "begin transaction")
  rezA <- use dbPool (insertPresentation prez)
  case rezA of
    Left err -> do
      use dbPool (sql "rollback transaction")
      pure (Left (show err))
    Right _ -> do
      -- do work here
      use dbPool (sql "commit transaction")
      pure (Right ())


insertPresentation :: Presentation -> Session ()
insertPresentation prez = do
  prjID <- createNewPresentation prez
  createLocales prjID prez.metadataP.localesPM
  createResources prjID prez.metadataP.resourcesPM
  createScenario prjID prez.scenarioP


createNewPresentation :: Presentation -> Session Int32
createNewPresentation prez =
  let
    md = prez.metadataP
  in
  statement (md.eidPM, md.namePM, md.notesPM) [TH.singletonStatement|
    insert into
      presentations (eid, pname, notes)
      values ($1::uuid, $2::text, $3::text)
    returning uid::int4
  |]


createLocales :: Int32 -> [Text] -> Session ()
createLocales prjId locales =
  mapM_ (\l -> statement (prjId, l) [TH.resultlessStatement|
    insert into prez_locales (prez_fk, locale) values ($1::int4, $2::text)
  |]) locales


createResources :: Int32 -> [Resource] -> Session ()
createResources prjId resources =
  mapM_ (\rz ->
      statement (prjId, rz.idR, resourceEnum rz.contentR, resourceContent rz.contentR) [TH.singletonStatement|
        insert into resources
          (prez_fk, eid, kind, content)
          values ($1::int4, $2::uuid, $3::text::resource_type, $4::text)
      |]
    ) resources


resourceEnum :: ResourceContent -> Text
resourceEnum rez = case rez of
  ImageRC _ -> "image"
  VideoRC _ -> "video"
  AudioRC _ -> "audio"
  Prop3DRC _ -> "prop3d"

resourceContent :: ResourceContent -> Text
resourceContent rez = case rez of
  ImageRC c -> c
  VideoRC c -> c
  AudioRC c -> c
  Prop3DRC c -> c


createScenario :: Int32 -> Scenario -> Session ()
createScenario prjID scenario = do
  scenarioID <- createNewScenario prjID
  mapM_ (uncurry (createAct scenarioID)) (zip [1..] scenario.actsS)


createNewScenario :: Int32 -> Session Int32
createNewScenario prjId =
  statement prjId [TH.singletonStatement|
    insert into scenarios
      (prez_fk)
      values ($1::int4)
    returning uid::int4
  |]


createAct :: Int32 -> Int32 -> Act -> Session ()
createAct scenarioID seqNbr act = do
  actID <- createNewAct scenarioID seqNbr
  mapM_ (uncurry (createScene actID)) (zip [1..] act.scenesA)


createNewAct :: Int32 -> Int32 -> Session Int32
createNewAct scenarioID seqNbr =
  statement (scenarioID, seqNbr) [TH.singletonStatement|
    insert into acts
      (scenario_fk, sequence_number)
      values ($1::int4, $2::int4)
    returning uid::int4
  |]


createScene :: Int32 -> Int32 -> Scene -> Session ()
createScene actID seqNbr scene = do
  sceneID <- createNewScene actID seqNbr scene.idS
  mapM_ (uncurry (createAction sceneID)) (zip [1..] scene.actionsS)


createNewScene :: Int32 -> Int32 -> UUID -> Session Int32
createNewScene actID seqNbr eid =
  statement (actID, seqNbr, eid) [TH.singletonStatement|
    insert into scenes
      (act_fk, sequence_number, eid)
      values ($1::int4, $2::int4, $3::uuid)
    returning uid::int4
  |]


createAction :: Int32 -> Int32 -> Action -> Session ()
createAction actID seqNbr action = do
  actionID <- createNewAction actID seqNbr (actionEnum action)
  case action of
    DialogAC d -> createDialogue actionID d
    ManipulationAC m -> createManipulation actionID m


createNewAction :: Int32 -> Int32 -> Text -> Session Int32
createNewAction actID seqNbr kind =
  statement (actID, seqNbr, kind) [TH.singletonStatement|
    insert into actions
      (scene_fk, sequence_number, kind)
      values ($1::int4, $2::int4, $3::text::action_type)
    returning uid::int4
  |]


actionEnum :: Action -> Text
actionEnum (DialogAC _) = "dialogue"
actionEnum (ManipulationAC _) = "manipulation"


createDialogue :: Int32 -> Dialogue -> Session ()
createDialogue actionID d =
  statement (actionID, d.actorDL, d.textDL, d.influenceDL) [TH.resultlessStatement|
    insert into dialogues
      (action_fk, actor, dtext, influence)
      values ($1::int4, $2::text, $3::text, $4::text?)
  |]


createManipulation :: Int32 -> Manipulation -> Session ()
createManipulation actionID m = do
  manipID <- createNewManipulation actionID m
  case m of
    LogicMN ld -> createLogicDef manipID ld
    PropMoveMN pm -> createPropMoveDef manipID pm


createNewManipulation :: Int32 -> Manipulation -> Session Int32
createNewManipulation actionID m =
  statement (actionID, manipulationEnum m) [TH.singletonStatement|
    insert into manipulations
      (action_fk, kind)
      values ($1::int4, $2::text::manipulation_type)
    returning uid::int4
  |]

manipulationEnum :: Manipulation -> Text
manipulationEnum (LogicMN _) = "logic"
manipulationEnum (PropMoveMN _) = "prop_move"


createLogicDef :: Int32 -> LogicDef -> Session ()
createLogicDef manipID ld =
  statement (manipID, ld.codeLD) [TH.resultlessStatement|
    insert into logic_defs
      (manipulation_fk, code)
      values ($1::int4, $2::text)
  |]


createPropMoveDef :: Int32 -> MovementDef -> Session ()
createPropMoveDef manipID md =
  statement (manipID, md.propMD, md.posMD) [TH.resultlessStatement|
    insert into movement_defs
      (manipulation_fk, prop, position)
      values ($1::int4, $2::text, $3::text)
  |]


locatePresentation :: UUID -> Session (Maybe Int32)
locatePresentation eid =
  statement eid [TH.maybeStatement|
    select uid::int4 from presentations where eid = $1::uuid
  |]


newtype TmpPrezEid = TmpPrezEid {
    eid :: UUID
  }
  deriving (Show, Eq, Generic, Ae.FromJSON)

prezEidFromValue :: Ae.Value -> Either String TmpPrezEid
prezEidFromValue aValue = case aValue of
  Ae.Object obj -> case Ae.fromJSON aValue of
    Ae.Success tmpPrezEid -> Right tmpPrezEid
    Ae.Error err -> Left $ "@[prezEidFromValue] failed to parse presentation ID: " <> err <> "."
  _ -> Left "@[prezEidFromValue] expected a JSON object with an 'eid' field."

-- | Fetch a presentation by its external ID
-- fetchPresentation :: Pool -> Text -> IO (Either String Presentation)
-- Rt.RunOptions -> Hp.Pool -> InternalArgs -> IO (Either String Value)
fetchPresentation :: NativeLibFunction
fetchPresentation dbPool (value, _) = do
  case prezEidFromValue value of
    Left err -> pure (Left err)
    Right prezEid -> do
      result <- use dbPool $ do
        maybePrezId <- locatePresentation prezEid.eid
        case maybePrezId of
          Nothing -> pure Nothing
          Just prezId -> findPresentation prezId
      case result of
        Left err -> pure (Left (show err))
        Right Nothing -> pure (Left "Presentation not found")
        Right (Just prez) -> pure (Right $ Ae.encode prez)

-- | Retrieve a complete presentation by its internal ID
findPresentation :: Int32 -> Session (Maybe Presentation)
findPresentation prezId = do
  maybeMetadata <- findPrezMetadata prezId
  case maybeMetadata of
    Nothing -> pure Nothing
    Just metadata -> do
      scenario <- fetchScenario prezId
      pure $ Just $ Presentation prezId metadata scenario

-- | Find presentation metadata including locales and resources
findPrezMetadata :: Int32 -> Session (Maybe PrezMetadata)
findPrezMetadata prezId = do
  maybeBaseData <- findPrezBaseData prezId
  case maybeBaseData of
    Nothing -> pure Nothing
    Just (idPM, namePM, notesPM) -> do
      localesPM <- fetchLocales prezId
      resourcesPM <- fetchResources prezId
      pure . Just $ PrezMetadata idPM namePM notesPM (V.toList localesPM) resourcesPM

-- | Find basic presentation data (id, name, notes)
findPrezBaseData :: Int32 -> Session (Maybe (UUID, Text, Text))
findPrezBaseData prezId =
  statement prezId [TH.maybeStatement|
    select eid::uuid, pname::text, notes::text
    from presentations
    where uid = $1::int4
  |]

-- | Fetch all locales for a presentation
fetchLocales :: Int32 -> Session (Vector Text)
fetchLocales prezId =
  statement prezId [TH.vectorStatement|
    select locale::text
    from prez_locales
    where prez_fk = $1::int4
    order by uid
  |]

-- | Fetch all resources for a presentation
fetchResources :: Int32 -> Session [Resource]
fetchResources prezId =
  toResourceList <$> statement prezId [TH.vectorStatement|
    select eid::uuid, kind::text, content::text
    from resources
    where prez_fk = $1::int4
    order by uid
  |]
  where
    toResourceList :: Vector (UUID, Text, Text) -> [Resource]
    toResourceList = V.toList . V.map (\(eid, kind, content) ->
      Resource eid (parseResourceContent kind content))

-- | Parse resource content from type and content text
parseResourceContent :: Text -> Text -> ResourceContent
parseResourceContent "image" content = ImageRC content
parseResourceContent "video" content = VideoRC content
parseResourceContent "audio" content = AudioRC content
parseResourceContent "prop3d" content = Prop3DRC content
parseResourceContent _ content = ImageRC content -- Default case

-- | Fetch scenario for a presentation
fetchScenario :: Int32 -> Session Scenario
fetchScenario prezId = do
  scenarioId <- findScenarioId prezId
  acts <- fetchActs scenarioId
  pure $ Scenario acts

-- | Find scenario ID for a presentation
findScenarioId :: Int32 -> Session Int32
findScenarioId prezId =
  statement prezId [TH.singletonStatement|
    select uid::int4
    from scenarios
    where prez_fk = $1::int4
  |]

-- | Fetch all acts for a scenario
fetchActs :: Int32 -> Session [Act]
fetchActs scenarioId = do
  actIds <- statement scenarioId [TH.vectorStatement|
    select uid::int4
    from acts
    where scenario_fk = $1::int4
    order by sequence_number
  |]
  mapM fetchAct (V.toList actIds)

-- | Fetch a single act with its scenes
fetchAct :: Int32 -> Session Act
fetchAct actId = do
  scenes <- fetchScenes actId
  pure $ Act scenes

-- | Fetch all scenes for an act
fetchScenes :: Int32 -> Session [Scene]
fetchScenes actId = do
  sceneDetails <- statement actId [TH.vectorStatement|
    select uid::int4, eid::uuid
    from scenes
    where act_fk = $1::int4
    order by sequence_number
  |]
  mapM (\(sceneId, sceneEid) -> do
    actions <- fetchActions sceneId
    pure $ Scene sceneEid actions) (V.toList sceneDetails)

-- | Fetch all actions for an act
fetchActions :: Int32 -> Session [Action]
fetchActions actId = do
  actionDetails <- statement actId [TH.vectorStatement|
    select uid::int4, kind::text
    from actions
    where scene_fk = $1::int4
    order by sequence_number
  |]
  mapM fetchActionContent (V.toList actionDetails)

-- | Fetch content for a specific action based on its type
fetchActionContent :: (Int32, Text) -> Session Action
fetchActionContent (actionId, "dialogue") = do
  dialogue <- fetchDialogue actionId
  pure $ DialogAC dialogue
fetchActionContent (actionId, "manipulation") = do
  manipulationData <- fetchManipulation actionId
  pure $ ManipulationAC manipulationData
fetchActionContent _ =
  error "Unknown action type" -- This should be improved with proper error handling

-- | Fetch dialogue content for an action
fetchDialogue :: Int32 -> Session Dialogue
fetchDialogue actionId =
  uncurry3 Dialogue <$>statement actionId [TH.singletonStatement|
      select
        actor::text, dtext::text, influence::text?
      from dialogues
      where action_fk = $1::int4
    |]

-- | Fetch manipulation data for an action
fetchManipulation :: Int32 -> Session Manipulation
fetchManipulation actionId = do
  (manipId, manipType) <- statement actionId [TH.singletonStatement|
    select uid::int4, kind::text
    from manipulations
    where action_fk = $1::int4
  |]
  case manipType of
    "logic" -> do
      logicDef <- fetchLogicDef manipId
      pure $ LogicMN logicDef
    "prop_move" -> do
      moveDef <- fetchPropMoveDef manipId
      pure $ PropMoveMN moveDef
    _ -> error "Unknown manipulation type" -- This should be improved with proper error handling

-- | Fetch logic definition for a manipulation
fetchLogicDef :: Int32 -> Session LogicDef
fetchLogicDef manipId =
  LogicDef <$>statement manipId [TH.singletonStatement|
    select code::text
    from logic_defs
    where manipulation_fk = $1::int4
  |]

-- | Fetch movement definition for a manipulation
fetchPropMoveDef :: Int32 -> Session MovementDef
fetchPropMoveDef manipId =
  uncurry MovementDef <$> statement manipId [TH.singletonStatement|
    select prop::text, position::text
    from movement_defs
    where manipulation_fk = $1::int4
  |]


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

