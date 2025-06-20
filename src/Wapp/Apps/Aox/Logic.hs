module Wapp.Apps.Aox.Logic where

import qualified Data.ByteString.Lazy as Lbs
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Mp

import qualified Data.Aeson as Ae

import qualified Aox.Opers as Aop
import qualified Wapp.AppDef as Wd

import Wapp.Apps.Aox.Types

-- type NativeLibFunction = Hp.Pool -> InternalArgs -> IO (Either String Lbs.ByteString)

getUserMailboxes :: Wd.NativeLibFunction
getUserMailboxes dbPool (aeValue, mbLabel) = do
  case Ae.fromJSON aeValue :: Ae.Result UserInfo of
    Ae.Error err -> do
      pure . Left . show $ "Invalid user info: " <> err
    Ae.Success userInfo -> do
      rezA <- Aop.getUserMailboxes dbPool userInfo.nameUI
      case rezA of
        Left err -> pure . Left . show $ err
        Right mailboxes ->
          pure . Right $ Ae.encode mailboxes

