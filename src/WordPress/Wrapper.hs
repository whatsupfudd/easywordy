{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module WordPress.Wrapper where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
-- import Data.IORef (IORef, modifyIORef', readIORef, newIORef)
import qualified Data.Map as Mp

import Foreign.C.Types
import Foreign.C.String (newCAString)
import Foreign.Ptr (Ptr)

import qualified Language.C.Inline as C
import qualified Options.Runtime as Rt


C.context (C.baseCtx <> C.funCtx)

{- TODO: reflect on the best way to implement the internal ub_write support.
localUbWrite :: IORef ByteString -> Ptr CChar -> CULong -> IO CULong
localUbWrite buf ptr size = do
  -- Convert the C string to ByteString
  str <- B.packCStringLen (ptr, fromIntegral size)
  -- Append the new string to the buffer
  modifyIORef' buf (`B.append` str)
  -- Return the number of characters handled
  return size
-}

C.include "<sapi/embed/php_embed.h>"
C.include "ext/standard/info.h"
C.include "phpSupport.c"

-- | Global initialization routine, runs once per process.
initPhpContext :: IO ()
initPhpContext = do
  [C.block| void {
    initGlobalBuffer();

    php_embed_module.name = "haskell_sapi_1_0_0";
    php_embed_module.pretty_name = "Haskell SAPI 1.0.0";
    // php_embed_module.startup = haskellSapiStartup;
    // php_embed_module.shutdown = haskellSapiShutdown;
    php_embed_module.ub_write = haskellSapiUbWrite;
    php_embed_module.register_server_variables = haskellSapiRegisterVariables;
    // php_embed_module.send_headers = haskellSapiSendHeaders;
  } |]


shutdownPhp :: IO ()
shutdownPhp = 
  pure ()

-- | Request initialization routine, runs once per request.
initRequest :: IO ()
initRequest = 
  pure ()

shutdownRequest :: IO ()
shutdownRequest = 
  pure ()


invoke :: String -> IO ByteString
invoke aString = do
  {- TODO: reflect on the best way to implement the internal ub_write support.
  buf <- newIORef B.empty
  let ub_write = localUbWrite buf
  -}
  scriptString <- newCAString aString
  rezA <- [C.block| char * {
    int argc = 0;
    char *scriptStr[2];
    char **argv;
    scriptStr[0] = $(char * scriptString);
    zval returnVal;


    PHP_EMBED_START_BLOCK(argc, argv)

    if (zend_eval_string(scriptStr[0], (zval *)&returnVal, "Embedded PHP script") == FAILURE) {
      php_printf("@[testString]Zend_eval_string failed.\n");
    }

    // TODO: figure out how to use the returnVal.
    const char *retType = zend_zval_type_name(&returnVal);
    php_printf("@[testString]retType: %s\n", retType);

    PHP_EMBED_END_BLOCK()
    return getGlobalBuffer();
    } |]
  B.packCString rezA


invokeFile :: Rt.RunOptions -> String -> Mp.Map String String -> IO ByteString
invokeFile rtOpts urlPath argMap = do
  initRequest
  putStrLn $ "@[invokeFile] urlPath: " <> urlPath
  scriptFile <- newCAString $ rtOpts.wp.rootPath <> "/" <> urlPath

  rezA <- [C.block| char * {
    int argc = 1;
    char *scriptPath[2];
    char **argv = scriptPath;
    scriptPath[0] = $(char * scriptFile);

    initGlobalBuffer();
    php_embed_module.ub_write = haskellSapiUbWrite;
    php_embed_module.register_server_variables = haskellSapiRegisterVariables;

    // PHP_EMBED_START_BLOCK(argc, argv)

    // Using the php_embed_init() function directly instead of the PHP_EMBED_START_BLOCK macro:
    php_embed_init(argc, argv);
    zend_first_try {

      zval track_vars_array;

      php_register_variable("PHP_SELF", argv[0], NULL);
      php_register_variable("SCRIPT_FILENAME", argv[0], &track_vars_array);
      php_register_variable("HTTP_HOST", argv[0], &track_vars_array);

      zend_file_handle file_handle;
      zend_stream_init_filename(&file_handle, argv[0]);

      if (! php_execute_script(&file_handle)) {
        php_printf("Script exec failed.\n");
      }
    } zend_catch {
      /* int exit_status = EG(exit_status); */
    } zend_end_try();
    php_embed_shutdown();

    // PHP_EMBED_END_BLOCK()

    return getGlobalBuffer();
  } |]
  B.packCString rezA
  -- C.free rezA
  -- rezStr <- readIORef buf


{-
    /* php_embed_module.ub_write = $fun:(unsigned long (*ub_write)(const char *, unsigned long));  // Set your ub_write function */
    /* php_import_environment_variables(&track_vars_array); */

    char paramout[256];
    zend_string *fileName;
    fileName = zend_string_init(ZEND_STRL("/tmp/gaga.php"), 0);

    /*
    zval retval;
    ZVAL_STRING(&retval, "gaga-gougoug");

  	zval ini_value;
  	ZVAL_NEW_STR(&ini_value, zend_string_init(ZEND_STRL("Embed SAPI error:"), /* persistent */ 1));


    zend_eval_string("'abcdefghijklmnopqrstuvwyxz\n';", &retval, "Embedded PHP script");
    strncpy(paramout, Z_STRVAL(retval), Z_STRLEN(retval));
    paramout[Z_STRLEN(retval)] = '\0';
    */

    zend_string_release(fileName);

    /* return strlen(paramout); */
-}