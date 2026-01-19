{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Native.Loader
  ( loadWappNative
  ) where

import Control.Exception (bracketOnError)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IORef
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker

import Foreign.Ptr (FunPtr)

-- Keep handles alive so code is not unloaded while in use.
{-# NOINLINE pluginHandles #-}
pluginHandles :: IORef (Map String DL)
pluginHandles = unsafePerformIO (newIORef M.empty)

type InitPluginFun = IO ()

foreign import ccall "dynamic"
  mkInitPluginFun :: FunPtr InitPluginFun -> InitPluginFun

-- OS-specific extension:
pluginExt :: String
#if defined(darwin_HOST_OS)
pluginExt = "dylib"
#else
pluginExt = "so"
#endif

pluginFileName :: String -> FilePath
pluginFileName wappId = "libew-wapp-" <> wappId <> "." <> pluginExt

-- Root directory where Wapp plugin libraries live; configurable.
pluginsRoot :: FilePath
pluginsRoot = "/opt/easywordy/plugins"   -- or from config

loadWappNative :: String -> IO ()
loadWappNative wappId = do
  already <- M.member wappId <$> readIORef pluginHandles
  if already
    then pure ()  -- already loaded
    else do
      let path = pluginsRoot </> pluginFileName wappId
      h <- dlopen path [RTLD_NOW]
      -- If anything fails after this, close h:
      bracketOnError (pure h) dlclose $ \handle -> do
        sym <- dlsym handle "ew_plugin_init"
        let funPtr = sym :: FunPtr InitPluginFun
            initFun = mkInitPluginFun funPtr
        initFun  -- this calls ewPluginInit -> registerExports
        atomicModifyIORef' pluginHandles $ \m -> (M.insert wappId handle m, ())
