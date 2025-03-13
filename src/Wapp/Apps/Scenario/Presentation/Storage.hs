{-# LANGUAGE QuasiQuotes #-}

module Wapp.Apps.Scenario.Presentation.Storage where

import Data.Profunctor (dimap)
import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)

import Hasql.Session (Session, ResultError, statement, sql)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH
import qualified Hasql.Encoders as He
import qualified Hasql.Decoders as Hd

import Wapp.Apps.Scenario.Presentation.Types

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
  statement (md.idPM, md.namePM, md.notesPM) [TH.singletonStatement|
    insert into
      presentations (eid, pname, notes)
      values ($1::text, $2::text, $3::text)
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
          values ($1::int4, $2::text, $3::text::resource_type, $4::text)
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
  mapM_ (uncurry (createScene scenarioID)) (zip [1..] scenario.scenesS)


createNewScenario :: Int32 -> Session Int32
createNewScenario prjId =
  statement prjId [TH.singletonStatement|
    insert into scenarios
      (prez_fk)
      values ($1::int4)
    returning uid::int4
  |]


createScene :: Int32 -> Int32 -> Scene -> Session ()
createScene scenarioID seqNbr scene = do
  sceneID <- createNewScene scenarioID seqNbr
  mapM_ (uncurry (createAct scenarioID)) (zip [1..] scene.actsS)


createNewScene :: Int32 -> Int32 -> Session Int32
createNewScene scenarioID seqNbr =
  statement (scenarioID, seqNbr) [TH.singletonStatement|
    insert into scenes
      (scenario_fk, sequence_number)
      values ($1::int4, $2::int4)
    returning uid::int4
  |]

createAct :: Int32 -> Int32 -> Act -> Session ()
createAct sceneID seqNbr act = do
  actID <- createNewAct sceneID seqNbr act.idA
  mapM_ (uncurry (createAction actID)) (zip [1..] act.actionsA)


createNewAct :: Int32 -> Int32 -> Text -> Session Int32
createNewAct sceneID seqNbr actID =
  statement (sceneID, seqNbr, actID) [TH.singletonStatement|
    insert into acts
      (scene_fk, sequence_number, eid)
      values ($1::int4, $2::int4, $3::text)
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
      (act_fk, sequence_number, kind)
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


locatePresentation :: Text -> Session (Maybe Int32)
locatePresentation eid =
  statement eid [TH.maybeStatement|
    select uid::int4 from presentations where eid = $1::text
  |]


-- | Fetch a presentation by its external ID
fetchPresentation :: Pool -> Text -> IO (Either String Presentation)
fetchPresentation dbPool prezEid = do
  result <- use dbPool $ do
    maybePrezId <- locatePresentation prezEid
    case maybePrezId of
      Nothing -> pure Nothing
      Just prezId -> findPresentation prezId
  case result of
    Left err -> pure (Left (show err))
    Right Nothing -> pure (Left "Presentation not found")
    Right (Just prez) -> pure (Right prez)

-- | Retrieve a complete presentation by its internal ID
findPresentation :: Int32 -> Session (Maybe Presentation)
findPresentation prezId = do
  maybeMetadata <- findPrezMetadata prezId
  case maybeMetadata of
    Nothing -> pure Nothing
    Just metadata -> do
      scenario <- fetchScenario prezId
      pure $ Just $ Presentation metadata scenario

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
findPrezBaseData :: Int32 -> Session (Maybe (Text, Text, Text))
findPrezBaseData prezId =
  statement prezId [TH.maybeStatement|
    select eid::text, pname::text, notes::text
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
    select eid::text, kind::text, content::text
    from resources
    where prez_fk = $1::int4
    order by uid
  |]
  where
    toResourceList :: Vector (Text, Text, Text) -> [Resource]
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
  scenes <- fetchScenes scenarioId
  pure $ Scenario scenes

-- | Find scenario ID for a presentation
findScenarioId :: Int32 -> Session Int32
findScenarioId prezId =
  statement prezId [TH.singletonStatement|
    select uid::int4
    from scenarios
    where prez_fk = $1::int4
  |]

-- | Fetch all scenes for a scenario
fetchScenes :: Int32 -> Session [Scene]
fetchScenes scenarioId = do
  sceneIds <- statement scenarioId [TH.vectorStatement|
    select uid::int4
    from scenes
    where scenario_fk = $1::int4
    order by sequence_number
  |]
  mapM fetchScene (V.toList sceneIds)

-- | Fetch a single scene with its acts
fetchScene :: Int32 -> Session Scene
fetchScene sceneId = do
  acts <- fetchActs sceneId
  pure $ Scene acts

-- | Fetch all acts for a scene
fetchActs :: Int32 -> Session [Act]
fetchActs sceneId = do
  actDetails <- statement sceneId [TH.vectorStatement|
    select uid::int4, eid::text
    from acts
    where scene_fk = $1::int4
    order by sequence_number
  |]
  mapM (\(actId, actEid) -> do
    actions <- fetchActions actId
    pure $ Act actEid actions) (V.toList actDetails)

-- | Fetch all actions for an act
fetchActions :: Int32 -> Session [Action]
fetchActions actId = do
  actionDetails <- statement actId [TH.vectorStatement|
    select uid::int4, kind::text
    from actions
    where act_fk = $1::int4
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

