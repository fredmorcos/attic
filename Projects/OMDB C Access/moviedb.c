/*
 * Dependencies: libcurl, jansson, pthreads
 * Build: gcc -Wall -Wextra -std=c11 -g3 -O2 -pthread -lcurl -ljansson moviedb.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <curl/curl.h>
#include <jansson.h>

typedef struct {
  char *mem;
  size_t len;
} buffer_t;

static size_t update_buffer (void *contents,
                             size_t len,
                             size_t nmemb,
                             void *_buf) {
  buffer_t *buffer = _buf;
  char *new_mem = NULL;
  size_t newlen = buffer->len + len * nmemb;

  if (!(new_mem = realloc(buffer->mem, newlen))) {
    fprintf(stderr, "ERROR: realloc(): %s\n", strerror(errno));
    return -1;
  }

  buffer->mem = new_mem;
  memcpy(buffer->mem + buffer->len, contents, newlen);
  buffer->len = newlen;

  return len * nmemb;
}

static void *do_download(void *buffer) {
  CURL *curl = NULL;
  CURLcode res;
  void *ret = (void *) 1;

  const char *const url =
    "https://api.themoviedb.org/3/movie/550?"
    "api_key=21dfe870a9244b78b4ad0d4783251c63";

  if (!(curl = curl_easy_init())) {
    fprintf(stderr, "ERROR: curl_easy_init() failed.\n");
    goto leave1;
  }

  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, update_buffer);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, buffer);

  if ((res = curl_easy_perform(curl)) != CURLE_OK) {
    fprintf(stderr, "ERROR: curl_easy_perform(): %s\n",
            curl_easy_strerror(res));
    goto leave2;
  }

  ret = NULL;
 leave2:
  curl_easy_cleanup(curl);
 leave1:
  pthread_exit(ret);
}

int main (void) {
  pthread_t download_thread;
  int rc;

  buffer_t buf = { .mem = NULL, .len = 0 };

  json_t *root, *name, *rating;
  json_error_t json_err;

  int ret = EXIT_FAILURE;

  if (curl_global_init(CURL_GLOBAL_DEFAULT |
                       CURL_GLOBAL_SSL     |
                       CURL_GLOBAL_ACK_EINTR) != 0) {
    fprintf(stderr, "ERROR: curl_global_init() failed.\n");
    goto l1;
  }

  if ((rc = pthread_create(&download_thread, NULL, do_download, &buf))) {
    fprintf(stderr, "ERROR: pthread_create(): %s\n", strerror(rc));
    goto l1;
  }

  printf("Fired off download in thread...\n");
  pthread_join(download_thread, NULL);
  printf("Finished.\n");

  if (!(root = json_loadb(buf.mem, buf.len, 0, &json_err))) {
    fprintf(stderr, "ERROR: json_loadb() at line %d: %s\n",
            json_err.line, json_err.text);
    goto l2;
  }

  printf("Movie JSON data loaded successfully.\n");
  free(buf.mem);

  name = json_object_get(root, "original_title");
  if (!json_is_string(name)) {
    fprintf(stderr, "ERROR: Movie name not a string\n");
    goto l3;
  }

  rating = json_object_get(root, "vote_average");
  if (!json_is_real(rating)) {
    fprintf(stderr, "ERROR: Movie rating not a double\n");
    goto l3;
  }

  printf("Movie name: %s\n", json_string_value(name));
  printf("Movie rating: %.1f\n", json_real_value(rating));

  json_decref(root);
  curl_global_cleanup();
  goto l1_succ;

 l3:
  json_decref(root);
  return ret;
 l2:
  free(buf.mem);
 l1:
  return ret;

 l1_succ:
  return EXIT_SUCCESS;
}
