{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Wapp.Apps.GnuHealth.Logic where


import qualified Data.ByteString.Lazy as Lbs
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Mp

import qualified Data.Aeson as Ae

import qualified Wapp.Apps.GnuHealth.Opers as Ops
import qualified Wapp.AppDef as Wd

import Wapp.Apps.GnuHealth.Types

-- type NativeLibFunction = Hp.Pool -> InternalArgs -> IO (Either String Lbs.ByteString)

gnuhealth_conf_commands :: Wd.NativeLibFunction
gnuhealth_conf_commands dbPool (aeValue, mbLabel) = do
  case Ae.fromJSON aeValue :: Ae.Result TablePosition of
    Ae.Error err -> do
      pure . Left . show $ "Invalid user info: " <> err
    Ae.Success tablePosition -> do
      rezA <- Ops.gnuhealth_conf_commands_fetch dbPool tablePosition
      case rezA of
        Left err -> pure . Left . show $ err
        Right aRow ->
          pure . Right $ Ae.encode aRow