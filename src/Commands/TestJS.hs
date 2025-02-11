
{-# LANGUAGE DerivingVia #-}

module Commands.TestJS where

import Control.Exception (try, SomeException)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as Tio

import qualified Options.Runtime as Rto

import qualified Wapp.JSSupport as JSS

testJsCmd :: Int -> Text -> Rto.RunOptions -> IO ()
testJsCmd testID aPath rtOpts = do
  putStrLn "@[aTestCmd] starting."
  -- rezA <- runElmTest (unpack aPath)
  let
    strPath = unpack aPath
  rezA <- try $
    JSS.runElmTest "/Users/lhugo/Documents/LProjets/E4Revive/Pipeline/Intisar/Edu/Sites/fdl_z14l/build/js/app/index.js"
  case rezA of
    Left err -> do
      putStrLn $ "@[aTestCmd] err: " <> show (err :: SomeException)
    Right jsReturn -> do
      putStrLn $ "@[aTestCmd] finished."
      rezC <- try $
         Tio.writeFile "/tmp/result.html" jsReturn.result
      case rezC of
        Left err ->
          putStrLn $ "@[aTestCmd] show err: " <> show (err :: SomeException)
        Right _ ->
          pure ()
