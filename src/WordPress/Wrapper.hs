{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module WordPress.Wrapper where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.IORef (IORef, modifyIORef', readIORef, newIORef)

import Foreign.C.Types
import Foreign.C.String (newCAString)
import Foreign.Ptr (Ptr)

import qualified Language.C.Inline as C


C.context (C.baseCtx <> C.funCtx)

localUbWrite :: IORef ByteString -> Ptr CChar -> CULong -> IO CULong
localUbWrite buf ptr size = do
  -- Convert the C string to ByteString
  str <- B.packCStringLen (ptr, fromIntegral size)
  -- Append the new string to the buffer
  modifyIORef' buf (`B.append` str)
  -- Return the number of characters handled
  return size


C.include "<sapi/embed/php_embed.h>"
C.include "ext/standard/info.h"
C.include "phpSupport.c"


testPHP :: IO ByteString
testPHP = do
  -- buf <- newIORef B.empty  -- Create a new IORef ByteString to store the output
  -- let ub_write = localUbWrite buf

  scriptFile <- newCAString "/bwork/wrkspc/karlin/Projets/Fudd/EasyWordy/Lib/wordpress/wp-admin/install.php" -- index.php
  -- scriptFile <- newCAString "/tmp/test.php"

  rezA <- [C.block| char * {
    int argc = 1;
    char *scriptPath[2];
    char **argv = scriptPath;
    scriptPath[0] = $(char * scriptFile);

    initGlobalBuffer();
    php_embed_module.ub_write = haskell_sapi_ub_write;
    php_embed_module.register_server_variables = haskell_sapi_register_variables;

    PHP_EMBED_START_BLOCK(argc, argv)

    zval track_vars_array;

    php_register_variable("PHP_SELF", argv[0], NULL);
    php_register_variable("SCRIPT_FILENAME", argv[0], &track_vars_array);
    php_register_variable("HTTP_HOST", argv[0], &track_vars_array);

    zend_file_handle file_handle;
    zend_stream_init_filename(&file_handle, argv[0]);

    if (! php_execute_script(&file_handle)) {
      php_printf("Script exec failed.\n");
    }

    /* php_embed_module.ub_write = default_ub_write; */
    PHP_EMBED_END_BLOCK()
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