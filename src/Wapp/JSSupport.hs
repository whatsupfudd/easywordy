{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}

module Wapp.JSSupport where

import Control.Exception (evaluate)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as Bs
import qualified Data.Map as Mp
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as Tio
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.UUID (UUID, fromString)
import GHC.Generics ( Generic )

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Ak
-- import Data.Aeson ( FromJSON, ToJSON, Value, toJSON, fromJSON, encode, decode, object, (.=), eitherDecode )

import Hasql.Pool (Pool)

import Language.JavaScript.Inline
import Language.JavaScript.Inline.Core

import qualified Options.Runtime as Rt
import qualified Wapp.Apps.Scenario.Presentation.Storage as Ps
import qualified Wapp.Apps.Aox.Logic as Aox
import qualified Wapp.Apps.Scenario.Presentation.DbOps as Pt
import qualified Wapp.AppDef as Wd
import qualified Native.Registry as Nr

import Wapp.Types (JSExecSupport(..))

data JSReturn = JSReturn {
    result :: Text
    , content :: Text
    , container :: Maybe Text
  }
  deriving (Generic, Show, Ae.FromJSON)
  deriving FromJS via (Aeson JSReturn)


runElmTest :: String -> IO JSReturn
runElmTest (Aeson -> aPath) = do
  session <- newSession defaultConfig
  -- elmModule <- importMJS session aPath
  putStrLn "@[runElmTest] starting."
  rezA <- eval session [js|
    // console.warn("@[insideJS] JS starting.")
    const { Elm } = (await import($aPath)).default;
    let resolvePromise;
    let innerVal = new Promise((resolve) => {
      resolvePromise = resolve;
    });

    updateInternal = (aValue) => {
      // console.warn("v: " + aValue.slice(0,20))
      resolvePromise(aValue);
    }

    doTest = async (moduleName) => {
      // console.warn("@[doTest] JS doTest start.")
      const app = Elm[moduleName].init({ flags: "test" });
      app.ports.log && app.ports.log.subscribe(updateInternal);
      // console.warn("@[doTest] JS doTest end.")

      const value = await innerVal;
      return value
    }

    const result = await doTest("BuildPage")
    // console.warn("@[insideJS] JS ending, result: ", result)
    return result
  |]
  rsA <- evaluate rezA :: IO JSReturn
  closeSession session
  putStrLn "@[runElmTest] finishing."
  return rsA

newtype NativeError = NativeError {
    msg :: Text
  }
  deriving (Generic, Show, Ae.ToJSON)


initJS :: FilePath -> Text -> IO (Session, JSVal)
initJS libPath moduleName = do
  putStrLn $ "@[initJS] starting, libPath: " <> libPath <> ", moduleName: " <> unpack moduleName
  session <- newSession defaultConfig
  putStrLn $ "@[initJS] done session: " <> show session
  elmModule <- importMJS session libPath
  putStrLn $ "@[initJS] done elmModule."
  let
    mNameLBS = LBS.fromStrict . encodeUtf8 $ moduleName
  putStrLn $ "@[initJS] mNameLBS: " <> show mNameLBS
  rezA <- eval session [js|
    jsModName = "" + $mNameLBS
    // console.warn("@[initJS.eval] elmModule: ", $elmModule)
    // console.warn("@[initJS.eval] module: ", $elmModule.default)
    // console.warn("@[initJS.eval] mNameLBS: ", jsModName)

    const app = ($elmModule.default)['Elm'][jsModName].init({ flags: { "locale" : "en" } })

    // console.warn("@[initJS.eval] app: ", app)
    return app
  |]
  putStrLn $ "@[initJS] done eval session."
  rsA <- evaluate rezA :: IO JSVal
  putStrLn $ "@[initJS] done evaluate rezA."
  -- putStrLn $ "@[initJS] done; rez: " <> show rsA
  return (session, elmModule)

endJS :: Session -> IO ()
endJS session = do
  closeSession session


data ExecParams = ExecParams {
  package :: Text
  , action :: Text
  , rcpt :: Text
  , params :: Ae.Value
  }
  deriving (Show, Generic, Ae.FromJSON, Ae.ToJSON)
  deriving (FromJS, ToJS) via (Aeson ExecParams)


runElmFunction :: JSExecSupport -> Maybe Pool -> Text -> Text -> Wd.RequestParams -> IO JSReturn
runElmFunction jsSupport mbDb moduleName functionName requestParams = do

  putStrLn $ "@[runElmFunction] starting, moduleName: " <> unpack moduleName
      <> ", functionName: " <> unpack functionName
      <> ", requestParams: " <> show requestParams

  let
    libExec :: JSExecSupport -> ExecParams -> IO LBS.ByteString
    libExec jsSupport execParams = do
      putStrLn $ "@[libEx/0] execParams: " <> show execParams
      case mbDb of
        Just dbPool ->
          if Mp.notMember execParams.package jsSupport.hsLibs then
            -- Dynamic library system (new):
            let
              fctName = execParams.package <> "." <> execParams.action
            in do
            putStrLn $ "@[libExec] looking up native function: " <> unpack fctName
            target <- Nr.lookupNative fctName
            case target of
              Just fct -> do
                eiActs <- fct dbPool (execParams.params, Nothing)
                case eiActs of
                  Left err -> do
                    putStrLn $ "@[libExec] error fetching acts: " <> err
                    pure $ Ae.encode $ NativeError { msg = "@[libExec] " <> fctName <> " err acts: " <> pack err }
                  Right rez -> do
                    putStrLn $ "@[libExec] invocation rez: " <> show rez
                    pure rez
              Nothing -> do
                putStrLn $ "@[libExec] function not found: " <> unpack fctName
                pure $ Ae.encode $ NativeError { msg = "@[libExec] package not found: " <> execParams.package }
          else do
            -- Statically linked system (old):
            case Mp.lookup execParams.package jsSupport.hsLibs of
              Just lib -> do
                case Mp.lookup execParams.action lib of
                  Just fct -> do
                    eiActs <- fct dbPool (execParams.params, Nothing)
                    case eiActs of
                      Left err -> do
                        putStrLn $ "@[libEx/1] error fetching acts: " <> err
                        pure $ Ae.encode $ NativeError { msg = "@[libEx/1] err acts: " <> pack err }
                      Right rez -> do
                        putStrLn $ "@[libEx/2] acts: " <> show rez
                        pure rez
                  Nothing ->
                    let
                      errMsg = "@[libEx/5] package " <> execParams.package <> ", action not found: " <> execParams.action
                    in do
                    putStrLn $ unpack errMsg
                    pure $ Ae.encode $ NativeError { msg = errMsg }
              Nothing -> do
                putStrLn $ "@[libEx/3] package not found: " <> unpack execParams.package
                pure $ Ae.encode $ NativeError { msg = "@[libEx/3] package not found: " <> execParams.package }
        Nothing -> do
          putStrLn "@[libEx/4]: no database pool"
          pure $ Ae.encode $ NativeError { msg = "@[libEx/4]: no database pool" }
      -- putStrLn $ "@[libExec] jsonParams: " <> show jsonParams
    mNameLBS = LBS.fromStrict . encodeUtf8 $ moduleName
    fctNameLBS = LBS.fromStrict . encodeUtf8 $ functionName
    jsonParamsLBS = Ae.encode requestParams
    jsElmModule = jsSupport.jsModule
  jsLibExec <- export jsSupport.jsSession (libExec jsSupport)

  rezA <- eval jsSupport.jsSession [js|
      jsModName = "" + $mNameLBS
      const app = ($jsElmModule.default)['Elm'][jsModName].init({ "flags": { "locale" : "en" } })
      // console.warn("@[runElmFunction] app: ", app)

      let resolvePromise
      let innerVal = new Promise((resolve) => {
        resolvePromise = resolve
      });

      updateInternal = (aValue) => {
        resolvePromise(JSON.parse(aValue))
      }

      execHaskellFct = async (jsonParams) => {
        console.warn("@[execHaskellFct] jsonParams: ", jsonParams)
        const callParams = JSON.parse(jsonParams)
        const tResult = await $jsLibExec(callParams)
        const result = {"result": JSON.parse(tResult) }
        const recipient = callParams.rcpt
        app.ports.recvMsg && app.ports.recvMsg.send({ "event" : "return", "rcpt" : recipient, "params" : result })

      }

      invokeElm = async (fctName) => {
        app.ports.sendOutput && app.ports.sendOutput.subscribe(updateInternal)
        const jParams = JSON.parse($jsonParamsLBS + "")
        console.warn("@[runElmFunction] params: ", jParams)
        app.ports.sendMsg && app.ports.sendMsg.subscribe(execHaskellFct)
        app.ports.recvMsg && app.ports.recvMsg.send({ "event" : "invoke", "fct" : fctName , "params" : jParams })

        const value = await innerVal
        return value
      }
      // For some reason, doing an op of the haskell-initiated variable makes it proper
      // JS instead of <Buffer ...>.
      const strFctName = "" + $fctNameLBS
      const result = await invokeElm(strFctName)
      return result    
  |]
  -- putStrLn $ "@[runElmFunction] done; rez: " <> show rezA
  evaluate rezA :: IO JSReturn
