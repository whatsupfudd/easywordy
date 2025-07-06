module Wapp.Types where

import qualified Control.Monad.RWS.Lazy as Rws
import qualified Control.Concurrent.STM as Cs

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

import qualified Network.WebSockets as WS

import Language.JavaScript.Inline as Js

import qualified Hasql.Pool as Hp

import qualified Options.Runtime as Rt
import qualified Wapp.AppDef as Wd
import Wapp.AppDef (ResolvedApp, NativeLibFunction, RequestParams)
import Wapp.HtmxSupport (HxWsMessage)
import qualified Wapp.State as Ws


data ReferenceEnv = ReferenceEnv {
  runOpts :: Rt.RunOptions
  , pgPool :: Hp.Pool
  , params :: Mp.Map Text Text
  , srvUpdate :: Cs.TMVar ()
  , commChannel :: Cs.TVar FilePath
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


data ClientContext = ClientContext {
  session :: Ws.Session
  , liveApp :: Ws.LiveWapp
  , heap :: Mp.Map Text Text
  , status :: Status
  , jsSupport :: Maybe JSExecSupport
  , actionList :: [ JsExecBlock ]
  }

type JsExecBlock = (HxWsMessage, Text, RequestParams)

data JSExecSupport = JSExecSupport {
    jsSession :: Js.Session
  , jsModule :: Js.JSVal
  , hsLibs :: Wd.NativeLibMap
  }


data AppEvent =
  FileUpdateAE FilePath
  | OutOfBandReplyAE Lbs.ByteString
  | IdlingAE
  deriving (Show)

data ClientMessage = 
  ErrorCM String
  | EventCM AppEvent
  | MessageCM WS.DataMessage
  deriving (Show)


newtype ExecResult = ExecResult {
   code :: Int
 }


type AppContext = Rws.RWST ReferenceEnv (Set Message) ClientContext IO

