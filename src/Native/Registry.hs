module Native.Registry
  ( NativeFun
  , NativeFunMap
  , ABIResult(..)
  , registerExports
  , lookupNative
  , registerExportsWithABI
  , JSVal
  ) where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Word (Word32)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Aeson as Ae

import qualified Native.ABI as ABI

-- Whatever your actual signature is:
type JSVal = Ae.Value
type NativeFun = [JSVal] -> IO JSVal  -- placeholder

type NativeFunMap = HashMap Text NativeFun

{-# NOINLINE globalNativeMap #-}
globalNativeMap :: IORef NativeFunMap
globalNativeMap = unsafePerformIO (newIORef HM.empty)

registerExports :: [(Text, NativeFun)] -> IO ()
registerExports exports =
  atomicModifyIORef' globalNativeMap $ \m ->
    (foldl (\acc (k, v) -> HM.insert k v acc) m exports, ())

lookupNative :: Text -> IO (Maybe NativeFun)
lookupNative name = HM.lookup name <$> readIORef globalNativeMap

data ABIResult = ABICompatible | ABIIncompatible Word32

registerExportsWithABI :: Word32 -> [(Text, NativeFun)] -> IO ABIResult
registerExportsWithABI abi xs =
  if abi == ABI.nativeABI
    then registerExports xs >> pure ABICompatible
    else pure (ABIIncompatible abi)
