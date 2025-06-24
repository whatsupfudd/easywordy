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
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID, fromString)
import GHC.Generics ( Generic )

import qualified Data.Aeson as Ae
-- import Data.Aeson ( FromJSON, ToJSON, Value, toJSON, fromJSON, encode, decode, object, (.=), eitherDecode )

import Hasql.Pool (Pool)

import Language.JavaScript.Inline
import Language.JavaScript.Inline.Core

import qualified Options.Runtime as Rt
import qualified Wapp.Apps.Scenario.Presentation.Storage as Ps
import qualified Wapp.Apps.Aox.Logic as Aox
import qualified Wapp.Apps.Scenario.Presentation.DbOps as Pt

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


initJS :: FilePath -> Text -> IO (Session, JSVal)
initJS libPath moduleName = do
  putStrLn $ "@[initJS] starting, libPath: " <> libPath <> ", moduleName: " <> unpack moduleName
  session <- newSession defaultConfig
  elmModule <- importMJS session libPath
  let
    mNameLBS = LBS.fromStrict . encodeUtf8 $ moduleName
  rezA <- eval session [js|
    jsModName = "" + $mNameLBS
    // console.warn("@[initJS.eval] elmModule: ", $elmModule)
    // console.warn("@[initJS.eval] module: ", $elmModule.default)
    // console.warn("@[initJS.eval] mNameLBS: ", jsModName)

    const app = ($elmModule.default)['Elm'][jsModName].init({ flags: { "locale" : "en" } })

    // console.warn("@[initJS.eval] app: ", app)
    return app
  |]
  rsA <- evaluate rezA :: IO JSVal
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


runElmFunction :: JSExecSupport -> Maybe Pool -> Text -> Text -> Ae.Value -> IO JSReturn
runElmFunction jsSupport mbDb moduleName functionName jsonParams = do

  putStrLn $ "@[runElmFunction] starting, moduleName: " <> unpack moduleName
      <> ", functionName: " <> unpack functionName
      <> ", jsonParams: " <> show jsonParams

  let
    libExec :: JSExecSupport -> ExecParams -> IO LBS.ByteString
    libExec jsSupport execParams = do
      putStrLn $ "@[libExec] execParams: " <> show execParams
      case mbDb of
        Just dbPool -> do
          case Mp.lookup execParams.package jsSupport.hsLibs of
            Just lib -> do
              case Mp.lookup execParams.action lib of
                Just fct -> do
                  eiActs <- fct dbPool (execParams.params, Nothing)
                  case eiActs of
                    Left err -> do
                      putStrLn $ "@[libExec] error fetching acts: " <> err
                      pure $ LBS.fromStrict $ encodeUtf8 $ "ERROR: " <> pack err
                    Right rez -> do
                      putStrLn $ "@[libExec] acts: " <> show rez
                      pure rez
                Nothing ->
                  pure $ "@[libExec] package " <> (LBS.fromStrict . encodeUtf8) execParams.package <> ", action not found: " <> (LBS.fromStrict . encodeUtf8) execParams.action
                  {-- TMP: fake a fetchPresentation call, for app z14l.
                  do
                  let
                    fakeUuid = "c909e59a-e2f0-41d1-ac70-932dea279823"
                  putStrLn $ "@[libExec] jsParams: " <> show execParams
                  case fromString fakeUuid of
                    Nothing -> do
                      pure $ "@[libExec] error parsing UUID: " <> (LBS.fromStrict . encodeUtf8 . pack) fakeUuid
                    Just prezId ->
                      let
                        aValue = Ae.object [ "eid" Ae..= prezId ]
                      in do
                      eiPrez <- Ps.fetchPresentation dbPool (aValue, Nothing)
                      case eiPrez of
                        Left err -> do
                          putStrLn $ "@[libExec] error fetching presentation: " <> err
                          pure $ "ERROR: " <> (LBS.fromStrict . encodeUtf8 . pack) err
                        Right prez -> do
                          -- putStrLn $ "@[libExec] presentation: " <> show prez
                          pure prez
                  -}
            Nothing -> do
              putStrLn $ "@[libExec] package not found: " <> unpack execParams.package
              pure $ "@[libExec] package not found: " <> (LBS.fromStrict . encodeUtf8) execParams.package
        Nothing -> do
          putStrLn "@[libExec]: no database pool"
          pure "@[libExec]: no database pool"
      -- putStrLn $ "@[libExec] jsonParams: " <> show jsonParams
    mNameLBS = LBS.fromStrict . encodeUtf8 $ moduleName
    fctNameLBS = LBS.fromStrict . encodeUtf8 $ functionName
    jsonParamsLBS = Ae.encode jsonParams
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
