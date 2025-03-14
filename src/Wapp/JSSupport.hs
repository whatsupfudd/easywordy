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
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as Tio
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics ( Generic )

import Data.Aeson ( FromJSON, Value, toJSON, encode )

import Hasql.Pool (Pool)

import Language.JavaScript.Inline
import Language.JavaScript.Inline.Core

import qualified Wapp.Apps.Scenario.Presentation.Storage as Ps

import Wapp.Types (JSExecSupport(..))
data JSReturn = JSReturn {
    result :: Text
    , content :: Text
  }
  deriving (Generic, Show, FromJSON)
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


runElmFunction :: JSExecSupport -> Maybe Pool -> Text -> Text -> Value -> IO JSReturn
runElmFunction jsSupport mbDb moduleName functionName jsonParams = do

  putStrLn $ "@[runElmFunction] starting, moduleName: " <> unpack moduleName
      <> ", functionName: " <> unpack functionName
      <> ", jsonParams: " <> show jsonParams

  let
    libExec :: JSExecSupport -> JSVal -> IO LBS.ByteString
    libExec jsSupport jsonParams = do
      case mbDb of
        Just dbPool -> do
          eiPrez <- Ps.fetchPresentation dbPool "09c6bd60-61e1-4890-9f28-2d71248b2c51"
          case eiPrez of
            Left err -> do
              putStrLn $ "@[libExec] error fetching presentation: " <> err
              pure $ LBS.fromStrict $ encodeUtf8 $ "ERROR: " <> pack err
            Right prez -> do
              -- putStrLn $ "@[libExec] presentation: " <> show prez
              pure $ encode prez
        Nothing -> pure "@[libExec]: no database pool"
      -- putStrLn $ "@[libExec] jsonParams: " <> show jsonParams
    mNameLBS = LBS.fromStrict . encodeUtf8 $ moduleName
    fctNameLBS = LBS.fromStrict . encodeUtf8 $ functionName
    jsonParamsLBS = encode jsonParams
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

      doTest = async (fctName) => {
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
      const result = await doTest(strFctName)
      return result    
  |]
  -- putStrLn $ "@[runElmFunction] done; rez: " <> show rezA
  evaluate rezA :: IO JSReturn
