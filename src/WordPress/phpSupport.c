#include <string.h>
#include <stdlib.h>

#include <main/SAPI.h>
#include <main/php.h>
#include <main/php_variables.h>
#include <sapi/embed/php_embed.h>
#include "ext/standard/info.h"


/*
The macros meaning:
#define PHP_EMBED_START_BLOCK(x,y) { \
    php_embed_init(x, y); \
    zend_first_try {

#define PHP_EMBED_END_BLOCK() \
  } zend_catch { \
    // int exit_status = EG(exit_status); \
  } zend_end_try(); \
  php_embed_shutdown(); \
}
*/


typedef struct {
  char *t1;
  char *t2;
} ew_global_context;

ew_global_context ewGlobalContext = { "ew1", "ew2" };

typedef struct {
    char *data;
    size_t len;
    size_t cap;
  } Buffer;

static Buffer *globalBuffer = NULL;
#define CAP_INCREMENT 8192

static void initGlobalBuffer() {
  fprintf(stderr, "@[initGlobalBuffer] Starting.\n");
  fflush(stderr);
  if (globalBuffer == NULL) {
    globalBuffer = malloc(sizeof(Buffer));
    globalBuffer->data = malloc(CAP_INCREMENT);
    globalBuffer->len = 0;
    globalBuffer->cap = CAP_INCREMENT;
  }
  else {
    globalBuffer->len = 0;
    globalBuffer->data[0] = '\0';
  }
}


static char *getGlobalBuffer() {
  globalBuffer->len = 0;
  return globalBuffer->data;
}

static void freeGlobalBuffer() {
  if (globalBuffer) {
    free(globalBuffer->data);
    free(globalBuffer);
    globalBuffer = NULL;
  }
}


static size_t haskellSapiUbWrite(const char *str, size_t str_length)
{
  /*
  request_rec *r;
	php_struct *ctx;

	ctx = SG(server_context);
	r = ctx->r;
  */
  Buffer *buf = globalBuffer;

  while (buf->len + str_length >= buf->cap) {
    buf->cap += CAP_INCREMENT;
    buf->data = realloc(buf->data, buf->cap);
    if (!buf->data) return 0;  // realloc failed
  }
  memcpy(buf->data + buf->len, str, str_length);
  buf->len += str_length;
  buf->data[buf->len] = '\0';  // Null-terminate the string

	return str_length; /* we always consume all the data passed to us. */
}


/* MODULE INIT: */
int ew_module_init(struct _sapi_module_struct *sapi_module) {
  /* As per fpm: */
  fprintf(stderr, "@[ew_module_init] Starting.\n");
  fflush(stderr);
  return php_module_startup(sapi_module, NULL);
}

/* MODULE SHUTDOWN: */
int ew_module_shutdown(struct _sapi_module_struct *sapi_module) {
  /* As per main.c: */
  fprintf(stderr, "@[ew_module_shutdown] Done.\n");
  fflush(stderr);
  php_module_shutdown();
  return SUCCESS;
}

/* REQUEST INIT: */
int ew_request_init(void) {
  fprintf(stderr, "@[ew_request_init] Starting.\n");
  fflush(stderr);
  initGlobalBuffer();
  return SUCCESS;
}               

/* REQUEST SHUTDOWN: */
int ew_request_shutdown(void) {
  fprintf(stderr, "@[ew_request_shutdown] Done.\n");
  fflush(stderr);
  // freeGlobalBuffer();
  return SUCCESS;
}


size_t ew_UnbufWrite(const char *str, size_t str_length)
{
  Buffer *buf = globalBuffer;

  while (buf->len + str_length >= buf->cap) {
    buf->cap += CAP_INCREMENT;
    buf->data = realloc(buf->data, buf->cap);
    if (!buf->data) return 0;  // realloc failed
  }
  memcpy(buf->data + buf->len, str, str_length);
  buf->len += str_length;
  buf->data[buf->len] = '\0';  // Null-terminate the string

	return str_length; /* we always consume all the data passed to us. */
}


/* error handler */
void ew_errorHandler(int type, const char *errMsg, ...) {
  va_list args;

  fprintf(stderr, "@[ew_errorHandler] %s\n", errMsg);
  fflush(stderr);
  va_start(args, errMsg);
  php_error(type, errMsg, args);
  va_end(args);
}               


/* send headers handler */
/*
typedef struct {
	zend_llist headers;
	int http_response_code;
	unsigned char send_default_content_type;
	char *mimetype;
	char *http_status_line;
} sapi_headers_struct;
*/
int ew_sendHeaders(sapi_headers_struct *sapi_headers) {
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

    return SAPI_HEADER_DO_SEND;
}

void ew_sendOneHeader(sapi_header_struct *sapi_header, void *server_context) {
  if (sapi_header != NULL) {
    char tmpHeader[1024];
    if (sapi_header->header_len < sizeof(tmpHeader)) {
      memcpy(tmpHeader, sapi_header->header, sapi_header->header_len);
      tmpHeader[sapi_header->header_len] = '\0';
      fprintf(stderr, "@[ew_sendOneHeader] called: %s, srv_ctxt: %p.\n", tmpHeader, server_context);
    }
    else {
      fprintf(stderr, "@[ew_sendOneHeader] header_len: %ld > 1024.\n", sapi_header->header_len);
    }
    fflush(stderr);
  }
}

/* read POST data */
size_t ew_readPost(char *buffer, size_t count_bytes) {
  fprintf(stderr, "@[ew_readPost] %s\n", buffer);
  fflush(stderr);
  return 0;
}

/* read Cookies */
char *ew_readCookies() {
  fprintf(stderr, "@[ew_readCookies]\n");
  fflush(stderr);
  return "abc=123";
}

void addRegisterVar(HashTable *ht, char *key, char *value) {
  zend_string *key_str = zend_string_init_interned(key, strlen(key), 0);
  zval zString;
  ZVAL_STRING(&zString, value);
  zend_hash_update_ind(ht, key_str, &zString);
  zend_string_release_ex(key_str, 0);
}


/* register global http server variables */
void ew_registerServerVariables(zval *track_var_array) {
  /* TODO: show the zval content. */
  void *context = SG(server_context);
  fprintf(stderr, "@[ew_registerServerVariables] %p, srv_ctxt: %p.\n", track_var_array, context);
  fflush(stderr);

/* nginx/fpm-php installation:
array(43) {
  ["USER"]=> string(8) "www-data"
  ["HOME"]=> string(8) "/var/www"
  ["HTTP_PRIORITY"]=> string(6) "u=0, i"
  ["HTTP_ACCEPT_LANGUAGE"]=> string(14) "en-US,en;q=0.9"
  ["HTTP_ACCEPT_ENCODING"]=> string(23) "gzip, deflate, br, zstd"
  ["HTTP_SEC_FETCH_DEST"]=> string(8) "document"
  ["HTTP_SEC_FETCH_USER"]=> string(2) "?1"
  ["HTTP_SEC_FETCH_MODE"]=> string(8) "navigate"
  ["HTTP_SEC_FETCH_SITE"]=> string(4) "none"
  ["HTTP_ACCEPT"]=> string(135) "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*-/-*;q=0.8,application/signed-exchange;v=b3;q=0.7"
  ["HTTP_USER_AGENT"]=> string(117) "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36"
  ["HTTP_UPGRADE_INSECURE_REQUESTS"]=> string(1) "1"
  ["HTTP_SEC_CH_UA_PLATFORM"]=> string(7) ""macOS""
  ["HTTP_SEC_CH_UA_MOBILE"]=> string(2) "?0"
  ["HTTP_SEC_CH_UA"]=> string(64) ""Google Chrome";v="129", "Not=A?Brand";v="8", "Chromium";v="129""
  ["HTTP_CACHE_CONTROL"]=> string(8) "no-cache" 
  ["HTTP_PRAGMA"]=> string(8) "no-cache" 
  ["HTTP_HOST"]=> string(6) "chodov" 
  ["SCRIPT_FILENAME"]=> string(62) "/home/lhugo/Projets/Fudd/EasyWordy/Lib/wordpress/test_info.php" 
  ["REDIRECT_STATUS"]=> string(3) "200" 
  ["SERVER_NAME"]=> string(6) "chodov" 
  ["SERVER_PORT"]=> string(3) "443" 
  ["SERVER_ADDR"]=> string(14) "192.168.132.10" 
  ["REMOTE_USER"]=> string(0) "" 
  ["REMOTE_PORT"]=> string(5) "60773" 
  ["REMOTE_ADDR"]=> string(14) "192.168.132.69" 
  ["SERVER_SOFTWARE"]=> string(12) "nginx/1.18.0" 
  ["GATEWAY_INTERFACE"]=> string(7) "CGI/1.1" 
  ["HTTPS"]=> string(2) "on" 
  ["REQUEST_SCHEME"]=> string(5) "https" 
  ["SERVER_PROTOCOL"]=> string(8) "HTTP/2.0" 
  ["DOCUMENT_ROOT"]=> string(48) "/home/lhugo/Projets/Fudd/EasyWordy/Lib/wordpress" 
  ["DOCUMENT_URI"]=> string(14) "/test_info.php" 
  ["REQUEST_URI"]=> string(18) "/test_info.php?p=1" 
  ["SCRIPT_NAME"]=> string(14) "/test_info.php" 
  ["CONTENT_LENGTH"]=> string(0) "" 
  ["CONTENT_TYPE"]=> string(0) "" 
  ["REQUEST_METHOD"]=> string(3) "GET" 
  ["QUERY_STRING"]=> string(3) "p=1" 
  ["FCGI_ROLE"]=> string(9) "RESPONDER" 
  ["PHP_SELF"]=> string(14) "/test_info.php" 
  ["REQUEST_TIME_FLOAT"]=> float(1729008418.584006) 
  ["REQUEST_TIME"]=> int(1729008418) }
*/

    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_HOST", "ledna");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "SCRIPT_FILENAME", "/bwork/wrkspc/karlin/Projets/Fudd/EasyWordy/Lib/wordpress/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "DOCUMENT_ROOT", "/bwork/wrkspc/karlin/Projets/Fudd/EasyWordy/Lib/wordpress/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "DOCUMENT_URI", "/index.php");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REQUEST_URI", "/?p=1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SCRIPT_NAME", "/");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "QUERY_STRING", "p=1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REQUEST_METHOD", "GET");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "PHP_SELF", "/");

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
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_SEC_CH_UA", "Google Chrome;v=129, Not=A?Brand;v=8, Chromium;v=129");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_CACHE_CONTROL", "no-cache");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTP_PRAGMA", "no-cache");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "REDIRECT_STATUS", "200");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_ADDR", "192.168.132.106");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REMOTE_USER", "");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REMOTE_PORT", "60773");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "REMOTE_ADDR", "192.168.132.69");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "SERVER_SOFTWARE", "zhpr/1.18.0");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "GATEWAY_INTERFACE", "CGI/1.1");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "HTTPS", "off");

    addRegisterVar(Z_ARRVAL_P(track_var_array), "CONTENT_LENGTH", "");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "CONTENT_TYPE", "");
    addRegisterVar(Z_ARRVAL_P(track_var_array), "FCGI_ROLE", "RESPONDER");
}

/*
  Adding values to an array:
  if (zend_hash_next_index_insert(Z_ARRVAL_P(track_var_array), &value) == NULL) {
    fprintf(stderr, "@[ew_registerServerVariables] zend_hash_next_index_insert failed.\n");
    fflush(stderr);
    zend_string_efree(Z_STR(value));
  }
*/
/* Log message */
void ew_logMessage(const char *message, int syslog_type_int) {
  fprintf(stderr, "@[ew_logMessage] %s\n", message);
  fflush(stderr);
}


#define UrlEncodedContentType "application/x-www-form-urlencoded"
#define FormDataContentType "multipart/form-data"


void showZVal(zval *zv) {
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
      fprintf(stderr, "unknown kind %u of zv: %p\n", Z_TYPE_P(zv), zv);
      break;
  }
  fflush(stderr);
}
