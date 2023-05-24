#include <string.h>
#include <stdlib.h>

#include <main/SAPI.h>
#include <sapi/embed/php_embed.h>
#include "ext/standard/info.h"
  
typedef struct {
    char *data;
    size_t len;
    size_t cap;
  } Buffer;

static Buffer *globalBuffer = NULL;

static void initGlobalBuffer() {
  if (globalBuffer == NULL) {
    globalBuffer = malloc(sizeof(Buffer));
    globalBuffer->data = malloc(8192);
    globalBuffer->len = 0;
    globalBuffer->cap = 0;
  }
  else {
    globalBuffer->len = 0;
    globalBuffer->data[0] = '\0';
  }
}


static size_t haskell_sapi_ub_write(const char *str, size_t str_length)
{
	/* request_rec *r;
	php_struct *ctx;

	ctx = SG(server_context);
	r = ctx->r;
  */
  Buffer *buf = globalBuffer;

  while (buf->len + str_length >= buf->cap) {
    buf->cap += 8192;
    buf->data = realloc(buf->data, buf->cap);
    if (!buf->data) return 0;  // realloc failed
  }
  memcpy(buf->data + buf->len, str, str_length);
  buf->len += str_length;
  buf->data[buf->len] = '\0';  // Null-terminate the string

	return str_length; /* we always consume all the data passed to us. */
}

static char *getGlobalBuffer() {
  return globalBuffer->data;
}


static void
haskell_sapi_register_variables(zval *track_vars_array)
{
	char *key, *val;
	size_t new_val_len;

	php_register_variable_safe("PHP_SELF", "/", 1, track_vars_array);
	php_register_variable_safe("SCRIPT_FILENAME", "index.php", 9, track_vars_array);
	php_register_variable_safe("HTTP_HOST", "karlin", 6, track_vars_array);
	php_register_variable_safe("REQUEST_URI", "/", 1, track_vars_array);
}
