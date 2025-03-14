{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Wapp.AppDef where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID)

import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?), (.!=), parseIndexedJSON, withText)
import qualified Data.Aeson.KeyMap as Ak
import qualified Data.Aeson.Key as Ak

import GHC.Generics (Generic)

import qualified Hasql.Pool as Hp

import qualified Options.Runtime as Rt
import qualified DB.Connect as Db

data AppDef = AppDef {
  uid :: UUID
  , label :: Text
  , locales :: [Text]
  , rootPath :: FilePath
  , libs :: [ PairReference ]
  , functions :: [ FunctionDef ]
  , db :: Maybe DbDef
 }
 deriving (Generic, Show, FromJSON)


{-
instance FromJSON AppDef where
  parseJSON (Object o) = AppDef
    <$> o .: "uid"
    <*> o .: "label"
    <*> o .: "locales"
    <*> o .: "rootPath"
    <*> o .: "libs"
    <*> o .: "functions"
-}


data FunctionDef = FunctionDef {
  fid :: Text
  , action :: ActionDef
  , args :: [ArgDef]
 }
 deriving (Generic, Show)

instance FromJSON FunctionDef where
  parseJSON (Object o) = FunctionDef
    <$> o .: "id"
    <*> o .: "action"
    <*> o .:? "args" .!= []


data ActionDef =
  FileVerbatimAD FilePath
  | TemplateAD FilePath
  | FunctionAD PairReference
 deriving (Generic, Show)


data PairReference = PairReference {
  label :: Text
  , ident :: Text
  }
 deriving (Generic, Show)

instance FromJSON PairReference where
  parseJSON (Object o) =
    let
      (label, value) = head $ Ak.toList o
      ident = case value of
        String s -> s
        Null -> ""
        _ -> "[PairReference.parseJSON] Invalid fct value: " <> (pack . show) value
    in
      pure (PairReference (pack . Ak.toString $ label) ident)


instance FromJSON ActionDef where
  parseJSON (Object o) =
    o .:? "FileVerbatim" >>= \case
      Just filePath -> pure (FileVerbatimAD filePath)
      Nothing -> o .:? "Template" >>= \case
        Just filePath -> pure (TemplateAD filePath)
        Nothing -> o .:? "Function" >>= \case
          Just ident -> FunctionAD <$> parseJSON ident
          Nothing -> fail $ "Invalid action: " <> show o



data ArgDef =
  IntArgDef
  | TextArgDef
 deriving (Generic, FromJSON, Show)

type RoutingTable = Mp.Map Text RouteLogic

data DbDef = DbDef {
  dbname :: Text
  , host :: Text
  , user :: Text
  , password :: Text
  }
  deriving (Generic, Show, FromJSON)


data RouteLogic =
  ExecFileRL FilePath [ RouteArg ]
  | FunctionRL RouteFunction
 deriving (Generic, Show)


data RouteFunction =
  Internal InternalFunction
  | External ExternalFunction
 deriving (Generic)


instance Show RouteFunction where
  show rf = case rf of
    Internal _ -> "Internal"
    External _ -> "External"


type InternalArgs = (Value, Maybe Text)
type InternalFunction = Rt.RunOptions -> Hp.Pool -> InternalArgs -> IO (Either String FunctionReply)
type ExternalFunction = (Text, Text, Text)


data FunctionReply =
  BasicFR Lbs.ByteString
  | AppendChildFR Lbs.ByteString
  | AfterEndFR Lbs.ByteString
 deriving (Generic, Show)


data RouteArg =
  IntRA Int32
  | TextRA Text
 deriving (Show, Generic, FromJSON)


-- Ready for execution, fully dereferenced version of an app:
data ResolvedApp = ResolvedApp {
  aid :: UUID
  , label :: Text
  , locales :: [Text]
  , rootPath :: FilePath
  , libs :: [ PairReference ]
  , functions :: Mp.Map Text RouteLogic
  , db :: Maybe Db.PgDbConfig
 }
 deriving (Show)

-- Support for doing the resolution of resources referred to by an app definition:
data FctResolver = FctResolver {
  resolveLib :: PairReference -> Either String PairReference
  , resolveFct :: Mp.Map Text Text ->FunctionDef -> Either String (Text, RouteLogic)
  }


type RoutingDictionary = Mp.Map UUID ResolvedApp
