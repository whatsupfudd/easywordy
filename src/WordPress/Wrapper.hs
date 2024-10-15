{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

module WordPress.Wrapper where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Mp
import Data.List as L (intercalate, concatMap)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.FilePath ((</>))


import GHC.Generics (Generic)

import Foreign (Storable (..), free)
import Foreign.C.Types
import Foreign.C.String (newCAString, CString)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)


import qualified Language.C.Inline as C
import qualified Options.Runtime as Rt
import Network.WebSockets (RequestHead(requestSecure))
import Data.Text.Internal.Read (IParser(P))


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
C.include "<sapi/embed/php_embed.h>"
-}


C.include "<main/SAPI.h>"
C.include "ext/standard/info.h"
C.include "phpSupport.c"


-- Revised php support:

defineSapiModuleStruct =
  [C.block| void * {
    void (*send_one_header)(sapi_header_struct *, void *) = (void (*)(sapi_header_struct *, void *))$fun:(void (*tSendOneHeader)(void *, void *));
    int (*send_headers)(sapi_headers_struct *) = (int (*)(sapi_headers_struct *))$fun:(int (*tSendHeaders)(void *));
    void (*register_srv_vars)(zval *) = (void (*)(zval *))$fun:(void (*tRegisterSrvVar)(void *));


    sapi_module_struct ew_sapi_module_def = {
      "easyWordy/1.0.0"          /* name */
      , "EasyWordy SAPI v1.0.0"       /* pretty_name */
      , ew_module_init                /* MINIT */
      , ew_module_shutdown            /* MSHUTDOWN */
      , ew_request_init               /* RINIT */
      , ew_request_shutdown           /* RSHUTDOWN */
      , ew_UnbufWrite                 /* Unbuffered Write */
      , NULL                          /* Flush */
      , NULL                          /* get uid */
      , NULL                          /* getenv */
      , php_error                     /* error handler: ew_errorHandler */
      , NULL                          /* header handler */
      , NULL                          /* send headers handler: ew_sendHeaders */
      , ew_sendOneHeader              /* send header handler:  */
      , ew_readPost                   /* read POST data:  */
      , ew_readCookies                /* read Cookies */
      , ew_registerServerVariables    /* register server variables:  */
      , ew_logMessage                 /* Log message:  */
      , NULL                          /* Get request time */
      , NULL                          /* Child terminate */
      , STANDARD_SAPI_MODULE_PROPERTIES
    };
    sapi_module_struct *sapiModuleStruct = malloc(sizeof(sapi_module_struct));
    memcpy(sapiModuleStruct, &ew_sapi_module_def, sizeof(sapi_module_struct));
    return sapiModuleStruct;
  }
  |]


beginPhp :: IO ()
beginPhp = do
  sapiModuleDef <- defineSapiModuleStruct
  initSapiForEasyWordy sapiModuleDef
  initPhp sapiModuleDef
  free sapiModuleDef

endPhp :: IO ()
endPhp = do
  cleanupPhp
  cleanupSapi


initSapiForEasyWordy :: Ptr () -> IO ()
initSapiForEasyWordy sapiModuleStruct = do
  putStrLn "@[initSapiForEasyWordy] Starting."
  [C.block| void {
    sapi_startup($(void * sapiModuleStruct));
  }
  |]


cleanupSapi :: IO ()
cleanupSapi =
  [C.block| void {
    sapi_shutdown();
  }
  |]


initPhp :: Ptr () -> IO ()
initPhp sapiModuleStruct = do
  putStrLn "@[initPhp] Starting."
  -- Duplicate of ew_module_init.
  [C.block| void {
    php_module_startup($(void * sapiModuleStruct), NULL);
    SG(server_context) = (void *)&ewGlobalContext;
  }
  |]


cleanupPhp :: IO ()
cleanupPhp =
  -- Duplicate of ew_module_shutdown.
  [C.block| void {
    php_module_shutdown();
  }
  |]


invokeFile :: Rt.RunOptions -> String -> Mp.Map String String -> IO (ByteString, NominalDiffTime)
invokeFile rtOpts urlPath argMap = do
  putStrLn $ "@[invokeFile] urlPath: " <> urlPath
  scriptFile <- newCAString $ rtOpts.wp.rootPath <> "/" <> urlPath

  initRequest rtOpts.wp.rootPath urlPath (intercalate "&" $ map (\(k,v) -> k <> "=" <> v) (Mp.toList argMap))
  startTime <- getCurrentTime
  [C.block| void {

    zend_first_try {
      zend_file_handle file_handle;

      fprintf(stderr, "@[invokeFile] scriptFile: %s\n", $(char * scriptFile));
      fflush(stderr);

      zend_stream_init_filename(&file_handle, $(char * scriptFile));

      if (!php_execute_script(&file_handle)) {
        fprintf(stderr, "@[invokeFile] Script exec failed.\n");
      }
      else {
        fprintf(stderr, "@[invokeFile] Script exec succeeded.\n");
      }
    } zend_catch {
      /* int exit_status = EG(exit_status); */
    } zend_end_try();

    fprintf(stderr, "@[invokeFile] Done.\n");
    fflush(stderr);
  } 
  |]
  endRequest
  endTime <- getCurrentTime
  let elapsedTime = endTime `diffUTCTime` startTime
  putStrLn $ "@[invokeFile] time: " <> show elapsedTime
  (,) <$> flushTextBuffer <*> pure elapsedTime


initRequest :: String -> String -> String -> IO () 
initRequest prefix postfix queryStr = do
  fullPath <- newCAString $  prefix </> postfix
  requestUri <- newCAString $ postfix <> "?" <> queryStr

  putStrLn "@[initRequest] Starting.";
  [C.block| void {

    const char HARDCODED_INI[] =
      "html_errors=0\n"
      "register_argc_argv=1\n"
      "implicit_flush=1\n"
      "output_buffering=0\n"
      "max_execution_time=0\n"
      "max_input_time=-1\n\0";
  
    zend_signal_startup();


  	php_embed_module.ini_entries = malloc(sizeof(HARDCODED_INI));
    memcpy(php_embed_module.ini_entries, HARDCODED_INI, sizeof(HARDCODED_INI));

    SG(request_info).request_method = "GET";
    SG(request_info).path_translated = $(char * fullPath);
    SG(request_info).request_uri = "/?p=1";
    SG(request_info).query_string = "p=1";
    SG(request_info).content_type = "text/html";
    SG(request_info).auth_user = NULL;
    SG(request_info).auth_password = NULL;
    SG(request_info).auth_digest = NULL;
    SG(request_info).argv0 = NULL;
    SG(request_info).current_user = NULL;
    SG(request_info).current_user_length = 0;
    SG(request_info).argc = 0;
    SG(request_info).argv = NULL;
    SG(request_info).proto_num = 0;
    /*
    Env var:
    ex.: http://ledna:8885/info.php/test?a=b
    PATH_INFO : /test
    PATH_TRANSLATED : /docroot/php_files/test
    SCRIPT_NAME : /info.php
    REQUEST_URI : /info.php/test?a=b
    SCRIPT_FILENAME: /docroot/php_files/info.php
    QUERY_STRING: a=b
    */


    // From php_embed_init:
    SG(options) |= SAPI_OPTION_NO_CHDIR;
    SG(request_info).argc = 1;
    SG(request_info).argv = & $(char * fullPath);

    /* php_globals, struct _php_core_globals in php_globals.h, access with PG(xxx):
      -- zval http_globals[6]:
      #define TRACK_VARS_POST		0
      #define TRACK_VARS_GET		1
      #define TRACK_VARS_COOKIE	2
      #define TRACK_VARS_SERVER	3
      #define TRACK_VARS_ENV		4
      #define TRACK_VARS_FILES	5
      #define TRACK_VARS_REQUEST	6
    */

    php_request_startup();

    zval *track_var_array = &PG(http_globals)[TRACK_VARS_SERVER];
    fprintf(stderr, "@[initRequest] http_globals: %p.\n", PG(http_globals));
    fprintf(stderr, "@[initRequest] track_var_array: %p.\n", track_var_array);
    fflush(stderr);

    /* showZVal(&PG(http_globals)[TRACK_VARS_ENV]); */

  }
  |]


endRequest :: IO ()
endRequest = do
  putStrLn "@[endRequest] Starting.";
  [C.block| void { php_request_shutdown(0); } |]


flushTextBuffer :: IO ByteString
flushTextBuffer = do
  rez <- B.packCString =<< [C.exp| char * { getGlobalBuffer() } |]
  -- putStrLn $ "@[flushTextBuffer] rez : " <> show rez
  [C.block| void { freeGlobalBuffer(); } |]
  if rez == "" then
    pure "<html><head><title>Empty flushTextBuffer</title></head><body><h1>Empty flushTextBuffer</h1></body></html>"
  else
    pure rez


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


-- TESTING:
-- A Haskell function that can be called from C. It receives a ptr to a string and a pointer to a zval (as void *).
tRegisterSrvVar :: Ptr () -> IO ()
tRegisterSrvVar tvArray = do
  [C.block| void {
    zval *track_var_array = $(void * tvArray);

    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_HOST", "ledna");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "SCRIPT_FILENAME", "/bwork/wrkspc/karlin/Projets/Fudd/EasyWordy/Lib/wordpress/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "DOCUMENT_ROOT", "/bwork/wrkspc/karlin/Projets/Fudd/EasyWordy/Lib/wordpress/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "DOCUMENT_URI", "/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REQUEST_URI", "/index.php?p=1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SCRIPT_NAME", "/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "QUERY_STRING", "p=1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REQUEST_METHOD", "GET");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "PHP_SELF", "/index.php");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "USER", "nhugo");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HOME", "/bwork/wrkspc/karlin");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_PRIORITY", "u=0, i");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_ACCEPT_LANGUAGE", "en-US,en;q=0.9");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_ACCEPT_ENCODING", "gzip, deflate, br, zstd");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_FETCH_DEST", "document");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_FETCH_USER", "?1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_FETCH_MODE", "navigate");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_FETCH_SITE", "none");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_NAME", "ledna");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_PORT", "8885");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REQUEST_SCHEME", "http");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_PROTOCOL", "HTTP/2.0");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_ACCEPT", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_USER_AGENT", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_UPGRADE_INSECURE_REQUESTS", "1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_CH_UA_PLATFORM", "macOS");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_CH_UA_MOBILE", "?0");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_CH_UA", "\"Google Chrome\";v=\"129\", \"Not=A?Brand\";v=\"8\", \"Chromium\";v=\"129\"");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_CACHE_CONTROL", "no-cache");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_PRAGMA", "no-cache");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "REDIRECT_STATUS", "200");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_ADDR", "192.168.132.106");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REMOTE_USER", "");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REMOTE_PORT", "60773");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REMOTE_ADDR", "192.168.132.69");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_SOFTWARE", "EZWD/1.18.0");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "GATEWAY_INTERFACE", "CGI/1.1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTPS", "off");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "CONTENT_LENGTH", "");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "CONTENT_TYPE", "");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "FCGI_ROLE", "RESPONDER");
    }
  |]


showZVal :: Ptr () -> IO ()
showZVal !zv =
  [C.block| void {
    zval *zv = $(void * zv);
    switch (Z_TYPE_P(zv)) {
      case IS_NULL:
        fprintf(stderr, "zv: NULL\n");
        break;
      case IS_LONG:
        fprintf(stderr, "zv: %ld\n", Z_LVAL_P(zv));
        break;
      case IS_DOUBLE:
        fprintf(stderr, "zv: %f\n", Z_DVAL_P(zv));
        break;
      case IS_STRING:
        fprintf(stderr, "zv: %s\n", Z_STRVAL_P(zv));
        break;
      case IS_ARRAY:
        fprintf(stderr, "zv: array\n");
        break;
      case IS_OBJECT:
        fprintf(stderr, "zv: object\n");
        break;
      case IS_RESOURCE:
        fprintf(stderr, "zv: resource\n");
        break;
      default:
        fprintf(stderr, "unknown kind %d of zv: %p\n", Z_TYPE_P(zv), zv);
        break;
    }
    fflush(stderr);
  }
  |]


tSendOneHeader :: Ptr () -> Ptr () -> IO ()
tSendOneHeader sapiHeader serverContext = do
  [C.block| void {
    sapi_header_struct *sapi_header = $(void * sapiHeader);
    void *server_context = $(void * serverContext);

    char tmpHeader[1024];
    if (sapi_header->header_len < sizeof(tmpHeader)) {
      memcpy(tmpHeader, sapi_header->header, sapi_header->header_len);
      fprintf(stderr, "@[ew_sendOneHeader] called: %s, srv_ctxt: %p.\n", tmpHeader, server_context);
    }
    else {
      fprintf(stderr, "@[ew_sendOneHeader] header_len: %ld > 1024.\n", sapi_header->header_len);
    }
    fflush(stderr);
  }
  |]


tSendHeaders :: Ptr () -> IO CInt
tSendHeaders sapiHeaders = do
  [C.block| int {
    sapi_headers_struct *sapi_headers = $(void * sapiHeaders);
    char *statusLine = "HTTP/1.0 200 OK\r\n";
    sapi_headers->http_status_line = emalloc(strlen(statusLine) + 1);
    strcpy(sapi_headers->http_status_line, statusLine);

    fprintf(stderr, "@[ew_sendHeaders] resp code: %d, status: %s\n", sapi_headers->http_response_code, sapi_headers->http_status_line);
    fprintf(stderr, "@[ew_sendHeaders] mimetype: %s\n", sapi_headers->mimetype);

    zend_llist *headers = &sapi_headers->headers;
  	zend_llist_position pos;

    for (zend_llist_element *element = zend_llist_get_first_ex(headers, &pos); element != NULL; element = zend_llist_get_next_ex(headers, &pos)) {
      sapi_header_struct *header = (sapi_header_struct *)element;
      fprintf(stderr, "@[ew_sendHeaders] header: %s\n", header->header);
    }
    fflush(stderr);

    return SAPI_HEADER_SENT_SUCCESSFULLY;
  }
  |]

