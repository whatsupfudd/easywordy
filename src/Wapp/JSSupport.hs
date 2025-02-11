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
import Data.Text (Text, unpack)
import qualified Data.Text.IO as Tio
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics ( Generic )

import Data.Aeson ( FromJSON )

import Language.JavaScript.Inline
import Language.JavaScript.Inline.Core


newtype JSReturn = JSReturn {
    result :: Text
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
      return { "result": value }
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
    console.warn("@[initJS] module: ", $elmModule.default)
    // console.warn("@[initJS] mNameLBS: ", $mNameLBS)

    const app = ($elmModule.default)['Elm'][$mNameLBS].init({ flags: "test" })
    return app
  |]
  rsA <- evaluate rezA :: IO JSVal
  return (session, elmModule)


runElmFunction :: Session -> JSVal -> Text -> Text -> IO JSReturn
runElmFunction session elmModule moduleName functionName = do
  putStrLn $ "@[runElmFunction] starting, moduleName: " <> unpack moduleName <> ", functionName: " <> unpack functionName
  let
    fctNameLBS = LBS.fromStrict . encodeUtf8 $ functionName
    mNameLBS = LBS.fromStrict . encodeUtf8 $ moduleName
  rezA <- eval session [js|
      const app = ($elmModule.default)['Elm'][$mNameLBS].init({ flags: "test" })
  
      let resolvePromise
      let innerVal = new Promise((resolve) => {
        resolvePromise = resolve
      });

      updateInternal = (aValue) => {
        resolvePromise(aValue)
      }

      doTest = async (fctName) => {
        app.ports.log && app.ports.log.subscribe(updateInternal)
        app.ports.recvMsg && app.ports.recvMsg.send("doPage")

        const value = await innerVal
        return { "result": value }
      }

      const result = await doTest($fctNameLBS)
      return result    
    |]
  evaluate rezA :: IO JSReturn
