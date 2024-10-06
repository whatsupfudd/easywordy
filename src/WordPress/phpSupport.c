#include <string.h>
#include <stdlib.h>

#include <main/SAPI.h>
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
    char *data;
    size_t len;
    size_t cap;
  } Buffer;

static Buffer *globalBuffer = NULL;
#define CAP_INCREMENT 8192

static void initGlobalBuffer() {
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


void initializeSapiContext() {
  /* ChatGPT suggested:
    sapi_startup(&my_sapi_module);
    if (my_sapi_module.startup) {
        my_sapi_module.startup(&my_sapi_module);
    }
  */
  SG(server_context) = NULL;
  // What about this: php_embed_init(); ?
}


#define UrlEncodedContentType "application/x-www-form-urlencoded"
#define FormDataContentType "multipart/form-data"


void setupRequestInfo(char *method
          , char *queryString, char *requestUri, char *pathTranslated
          , size_t responseCode
          , int contentType, size_t contentLength) {


    SG(request_info).request_method = estrdup(method);
    SG(request_info).query_string = estrdup(queryString);
    SG(request_info).request_uri = estrdup(requestUri);
    SG(request_info).path_translated = estrdup(pathTranslated);
    // For POST request:
    if (strcmp(method, "POST") == 0) {
      if (contentType == 0) {
        SG(request_info).content_type = estrdup(UrlEncodedContentType);
      }
      else {
        SG(request_info).content_type = estrdup(FormDataContentType);
      }
      SG(request_info).content_length = contentLength;
    }
    else {
      SG(request_info).content_type = estrdup("");
      SG(request_info).content_length = 0;
    }
    // TODO: handle other kinds of requests.
    SG(sapi_headers).http_response_code = responseCode;
}


int haskellSapiStartup(struct _sapi_module_struct *sapi_module) {
  fprintf(stderr, "starting haskellSapiStartup.\n");
  sapi_startup(sapi_module);
  return SUCCESS;
}

int haskellSapiShutdown() {
  fprintf(stderr, "starting haskellSapiShutdown.\n");
  sapi_shutdown();
  return SUCCESS;
}


void populateGetVariables() {
  fprintf(stderr, "starting populateGetVariables.\n");
  // sapi_module.treat_data(PARGE_GET, estrdup(SG(request_info). query_string), NULL, TSRMLS_CC);
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


static void haskellSapiRegisterVariables(zval *track_vars_array)
{
	char *key, *val;
	size_t new_val_len;

	php_register_variable_safe("PHP_SELF", "/", 1, track_vars_array);
	php_register_variable_safe("SCRIPT_FILENAME", "index.php", 9, track_vars_array);
	php_register_variable_safe("HTTP_HOST", "karlin", 6, track_vars_array);
	php_register_variable_safe("REQUEST_URI", "/", 1, track_vars_array);
}

static int haskellSapiSendHeaders(sapi_headers_struct *sapi_headers) {
	return SAPI_HEADER_SENT_SUCCESSFULLY;
}

