{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Wapp.AppDef where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID)

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.:?), (.!=), parseIndexedJSON, withText, genericToJSON)
import qualified Data.Aeson.KeyMap as Ak
import qualified Data.Aeson.Key as Ak

import GHC.Generics (Generic)

import qualified Hasql.Pool as Hp

import qualified Options.Runtime as Rt
import qualified DB.Connect as Db
import qualified Wapp.HtmxSupport as Hx
import qualified Utils.Json as Uj

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


data RequestParams = RequestParams {
    hxParamsRP :: Maybe Value
    , formFieldsRP :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON RequestParams where
  parseJSON (Object obj) = RequestParams <$> obj .:? "jsonParams" <*> obj .:? "formFields"
  parseJSON _ = fail "Expected JSON Object for RequestParams"

instance ToJSON RequestParams where
  toJSON = genericToJSON Uj.cutFieldP2


type ClientArgs = (RequestParams, Maybe Text)
type NativeFctArgs = (Value, Maybe Text)
type InternalFunction = Rt.RunOptions -> Hp.Pool -> ClientArgs -> IO (Either String FunctionReply)
type NativeLibFunction = Hp.Pool -> NativeFctArgs -> IO (Either String Lbs.ByteString)
type ExternalFunction = (Text, Text, Text)

-- Map of packages = Map of Internal function name -> Haskell function (defined as Function: <package>: function_name in app yaml definition).
type LibraryMap = Mp.Map Text (Mp.Map Text InternalFunction)

-- Map of packages, each package -> map of hsForward name -> Haskell function:
type NativeLibMap = Mp.Map Text (Mp.Map Text NativeLibFunction)



data FunctionReply =
  BasicFR (Lbs.ByteString, Maybe Text)
  | AppendChildFR (Lbs.ByteString, Maybe Text)
  | AfterEndFR (Lbs.ByteString, Maybe Text)
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
  , resolveFct :: Mp.Map Text Text -> FunctionDef -> Either String (Text, RouteLogic)
  }


type RoutingDictionary = Mp.Map UUID ResolvedApp
