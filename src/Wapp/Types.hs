{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Wapp.Types where

import qualified Control.Monad.RWS.Lazy as Rws

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Set (Set (..))
import Data.Text (Text, unpack, pack)
import Data.UUID (UUID)

import GHC.Generics
import Data.Aeson (FromJSON (..), Value (..), (.:), (.:?), (.!=), parseIndexedJSON, withText)
import qualified Data.Aeson.KeyMap as Ak
import qualified Data.Aeson.Key as Ak

import Language.JavaScript.Inline as Js

import qualified Hasql.Pool as Hp

import qualified Options.Runtime as Rt


data ReferenceEnv = ReferenceEnv {
  runOpts :: Rt.RunOptions
  , pgPool :: Hp.Pool
  , params :: Mp.Map Text Text
  }


data Message =
  BinaryM Bs.ByteString
  | TextM Text
  | FileClone FilePath
 deriving (Show, Ord, Eq)


data Status =
  RunningST
  | HaltRequestST
  | HaltedST
  | ErrorST String


data ExecContext = ExecContext {
  session :: WsSession
  , resolvedApp :: ResolvedApp
  , heap :: Mp.Map Text Text
  , status :: Status
  , jsSupport :: Maybe JSExecSupport
  }

data JSExecSupport = JSExecSupport {
    jsSession :: Js.Session
  , jsModule :: Js.JSVal
  }

newtype ExecResult = ExecResult {
   code :: Int
 }


data User = User {
  userID :: UUID
  , profile :: UserProfile
  }


data UserProfile = UserProfile {
  name :: Text
  , email :: Text
  , avatar :: Maybe Text
  , prefLocale :: Text
  }


data WsSession = WsSession {
   user :: User
   , sessionID :: UUID
 }


type AppContext = Rws.RWST ReferenceEnv (Set Message) ExecContext IO

type RoutingDictionary = Mp.Map UUID ResolvedApp
data ResolvedApp = ResolvedApp {
  aid :: UUID
  , label :: Text
  , locales :: [Text]
  , rootPath :: FilePath
  , libs :: [ PairReference ]
  , functions :: Mp.Map Text RouteLogic
 }


data FctResolver = FctResolver {
  resolveLib :: PairReference -> Either String PairReference
  , resolveFct :: Mp.Map Text Text ->FunctionDef -> Either String (Text, RouteLogic)
  }


data AppDef = AppDef {
  uid :: UUID
  , label :: Text
  , locales :: [Text]
  , rootPath :: FilePath
  , libs :: [ PairReference ]
  , functions :: [ FunctionDef ]
 }
 deriving (Generic, Show)

instance FromJSON AppDef where
  parseJSON (Object o) = AppDef
    <$> o .: "uid"
    <*> o .: "label"
    <*> o .: "locales"
    <*> o .: "rootPath"
    <*> o .: "libs"
    <*> o .: "functions"


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

data RouteLogic =
  ExecFileRL FilePath [ RouteArg ]
  | FunctionRL RouteFunction
 deriving (Generic)


data RouteFunction =
  Internal InternalFunction
  | External ExternalFunction
 deriving (Generic)

data FunctionReply =
  BasicFR Lbs.ByteString
  | AppendChildFR Lbs.ByteString
  | AfterEndFR Lbs.ByteString
 deriving (Generic)


type InternalArgs = (Value, Maybe Text)
type InternalFunction = Rt.RunOptions -> Hp.Pool -> InternalArgs -> IO (Either String FunctionReply)
type ExternalFunction = (Text, Text, Text)

data RouteArg =
  IntRA Int32
  | TextRA Text
 deriving (Show, Generic, FromJSON)

