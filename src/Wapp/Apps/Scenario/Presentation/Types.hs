{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Wapp.Apps.Scenario.Presentation.Types where

import Control.Applicative

import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID)

import GHC.Generics
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae


-- | Root level project container.
data Presentation = Presentation {
    uidP :: Int32
    , metadataP :: PrezMetadata
    , scenarioP :: Scenario
  }
  deriving (Show, Eq, Generic)

instance Ae.FromJSON Presentation where
  parseJSON = Ae.genericParseJSON cutFieldP1
instance Ae.ToJSON Presentation where
  toJSON = Ae.genericToJSON cutFieldP1


-- | Presentation metadata information.
data PrezMetadata = PrezMetadata {
    idPM :: UUID
  , namePM :: Text
  , notesPM :: Text
  , localesPM :: [Text]
  , resourcesPM :: [Resource]
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON PrezMetadata where
  parseJSON = Ae.genericParseJSON cutFieldP2
instance Ae.ToJSON PrezMetadata where
  toJSON = Ae.genericToJSON cutFieldP2


-- | Resources (images, videos, etc) being associated with an internal ID for quick reference
-- in the rest of the content.
data Resource = Resource {
    idR :: UUID
  , contentR :: ResourceContent
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON Resource where
  parseJSON = Ae.genericParseJSON cutFieldP1
instance Ae.ToJSON Resource where
  toJSON = Ae.genericToJSON cutFieldP1

-- | The kind of resources that can be referred to.
data ResourceContent = 
  ImageRC Text
  | VideoRC Text
  | AudioRC Text
  | Prop3DRC Text
  deriving (Show, Eq, Generic)


-- | Scenario representing the full presentation flow.
newtype Scenario = Scenario { 
    scenesS :: [Scene]
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON Scenario where
  parseJSON = Ae.genericParseJSON cutFieldP1
instance Ae.ToJSON Scenario where
  toJSON = Ae.genericToJSON cutFieldP1


-- | Scene: a collection of acts where actors & props are stable.
newtype Scene = Scene {
    actsS :: [Act]
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON Scene where
  parseJSON = Ae.genericParseJSON cutFieldP1
instance Ae.ToJSON Scene where
  toJSON = Ae.genericToJSON cutFieldP1


-- | Act: a set actors/props situation, where we get dialogues, props manipulation. It
-- also defines the camera and lighting.
data Act = Act {
    idA :: UUID
  , actionsA :: [Action]
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON Act where
  parseJSON = Ae.genericParseJSON cutFieldP1
instance Ae.ToJSON Act where
  toJSON = Ae.genericToJSON cutFieldP1


-- | Action to be performed in an act
data Action = 
  DialogAC Dialogue
  | ManipulationAC Manipulation
  deriving (Show, Eq, Generic)


data Dialogue = Dialogue {
    actorDL :: Text
  , textDL :: Text
  , influenceDL :: Maybe Text
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON Dialogue where
  parseJSON = Ae.genericParseJSON cutFieldP2
instance Ae.ToJSON Dialogue where
  toJSON = Ae.genericToJSON Ae.defaultOptions {
    Ae.fieldLabelModifier = init . init
    , Ae.omitNothingFields = True
  }

data Manipulation = 
  LogicMN LogicDef
  | PropMoveMN MovementDef
  deriving (Show, Eq, Generic)


newtype LogicDef = LogicDef {
    codeLD :: Text
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON LogicDef where
  parseJSON = Ae.genericParseJSON cutFieldP2
instance Ae.ToJSON LogicDef where
  toJSON = Ae.genericToJSON cutFieldP2


data MovementDef = MovementDef {
    propMD :: Text
  , posMD :: Text
  }
  deriving (Show, Eq, Generic)
instance Ae.FromJSON MovementDef where
  parseJSON = Ae.genericParseJSON cutFieldP2
instance Ae.ToJSON MovementDef where
  toJSON = Ae.genericToJSON cutFieldP2


instance Ae.FromJSON ResourceContent where
  parseJSON = Ae.withObject "ResourceContent" $ \v -> do
    resourceType <- v .: "type"
    resourceValue <- v .: "value"
    case resourceType of
      "image" -> pure $ ImageRC resourceValue
      "video" -> pure $ VideoRC resourceValue
      "audio" -> pure $ AudioRC resourceValue
      "prop3d" -> pure $ Prop3DRC resourceValue
      _ -> fail $ "Unknown resource type: " ++ unpack resourceType

instance Ae.ToJSON ResourceContent where
  toJSON (ImageRC value) = Ae.object ["type" .= ("image" :: Text), "value" .= value]
  toJSON (VideoRC value) = Ae.object ["type" .= ("video" :: Text), "value" .= value]
  toJSON (AudioRC value) = Ae.object ["type" .= ("audio" :: Text), "value" .= value]
  toJSON (Prop3DRC value) = Ae.object ["type" .= ("prop3d" :: Text), "value" .= value]

-- | JSON instance for Action
instance Ae.FromJSON Action where
  parseJSON = Ae.withObject "Action" $ \v -> do
    actionType <- v .: "type"
    case actionType of
      "dialogue" -> DialogAC <$> Ae.parseJSON (Ae.Object v)
      "manipulation" -> do
        logic <- v .:? "logic" :: Ae.Parser (Maybe LogicDef)
        propMove <- v .:? "propMove" :: Ae.Parser (Maybe MovementDef)
        case (logic, propMove) of
          (Just _, Nothing) -> ManipulationAC . LogicMN <$> v .: "logic"
          (Nothing, Just _) -> ManipulationAC . PropMoveMN <$> v .: "propMove"
          _ -> fail "Manipulation must have either 'logic' or 'propMove', but not both or neither"
      _ -> fail $ "Unknown action type: " ++ unpack actionType

instance Ae.ToJSON Action where
  toJSON (DialogAC dialogue) = 
    let baseObj = Ae.toJSON dialogue
    in case baseObj of
         Ae.Object o -> Ae.Object $ o <> ("type" .= ("dialogue" :: Text))
         _ -> error "Unexpected non-object for Dialogue"
  
  toJSON (ManipulationAC manipulation) =
    case manipulation of
      LogicMN logic -> 
        Ae.object ["type" .= ("manipulation" :: Text), "logic" .= logic]
      PropMoveMN movement -> 
        Ae.object ["type" .= ("manipulation" :: Text), "propMove" .= movement]

-- | JSON instance for Manipulation
instance Ae.FromJSON Manipulation where
  parseJSON = Ae.withObject "Manipulation" $ \v -> do
    logic <- v .:? "logic" :: Ae.Parser (Maybe LogicDef)
    propMove <- v .:? "propMove" :: Ae.Parser (Maybe MovementDef)
    case (logic, propMove) of
      (Just _, Nothing) -> LogicMN <$> v .: "logic"
      (Nothing, Just _) -> PropMoveMN <$> v .: "propMove"
      _ -> fail "Manipulation must have either 'logic' or 'propMove', but not both or neither"

instance Ae.ToJSON Manipulation where
  toJSON (LogicMN logic) = Ae.object ["logic" .= logic]
  toJSON (PropMoveMN movement) = Ae.object ["propMove" .= movement]


cutFieldP1 :: Ae.Options
cutFieldP1 = Ae.defaultOptions {
    Ae.fieldLabelModifier = init
  }

cutFieldP2 :: Ae.Options
cutFieldP2 = Ae.defaultOptions {
    Ae.fieldLabelModifier = init . init
  }

