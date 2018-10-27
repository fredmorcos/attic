/*
 * Dependencies: libcurl, jansson, pthreads
 * Build: gcc -Wall -Wextra -std=c11 -g3 -O2 -pthread -lcurl -ljansson moviedb.c
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <err.h>
#include <errno.h>
#include <pthread.h>
#include <curl/curl.h>
#include <cJSON.h>

typedef void *(*tfunc)(void *);

static void *download(FILE *data) {
  assert(data);

  const char url[] =
    "https://api.themoviedb.org/3/movie/550?"
    "api_key=21dfe870a9244b78b4ad0d4783251c63";

  CURL *const curl = curl_easy_init();

  if (!curl) {
    warnx("cURL init failed");
    return NULL;
  }

  (void) curl_easy_setopt(curl, CURLOPT_URL, url);
  (void) curl_easy_setopt(curl, CURLOPT_WRITEDATA, data);

  const CURLcode res = curl_easy_perform(curl);

  if (res != CURLE_OK) {
    warnx("cURL perform error: %s", curl_easy_strerror(res));
    curl_easy_cleanup(curl);
    return NULL;
  }

  curl_easy_cleanup(curl);
  return data;
}

int main (void) {
  if (curl_global_init(CURL_GLOBAL_DEFAULT |
                       CURL_GLOBAL_SSL     |
                       CURL_GLOBAL_ACK_EINTR) != 0)
    err(EXIT_FAILURE, "cURL init failed");

  char *buf = NULL;
  size_t bufsize = 0;
  FILE *const stream = open_memstream(&buf, &bufsize);

  if (!stream)
    err(EXIT_FAILURE, "cannot open memstream");

  pthread_t thread;
  const int rc =
    pthread_create(&thread, NULL, (tfunc) download, (void *) stream);

  if (rc != 0) {
    warnx("cannot create thread: %s", strerror(rc));
    (void) fclose(stream);
    curl_global_cleanup();
    return EXIT_FAILURE;
  }

  printf("Fired off download in thread...\n");
  (void) pthread_join(thread, NULL);

  if (fwrite("\0", 1, 1, stream) < 1) {
    warn("cannot write terminating byte to stream");
    (void) fclose(stream);
    curl_global_cleanup();
    free(buf);
    return EXIT_FAILURE;
  }

  (void) fclose(stream);
  curl_global_cleanup();
  printf("Finished.\n");

  cJSON *const root = cJSON_Parse(buf);
  free(buf);

  if (!root)
    errx(EXIT_FAILURE, "cannot parse stream: %s", cJSON_GetErrorPtr());

  printf("Movie JSON: %s\n", cJSON_Print(root));
  printf("Movie JSON data loaded successfully.\n");

  cJSON *obj = cJSON_GetObjectItem(root, "original_title");

  if (!obj) {
    warnx("cannot find movie title: %s", cJSON_GetErrorPtr());
    cJSON_Delete(root);
    return EXIT_FAILURE;
  }

  printf("Movie name: %s\n", obj->valuestring);

  obj = cJSON_GetObjectItem(root, "vote_average");

  if (!obj) {
    warnx("cannot find movie rating: %s", cJSON_GetErrorPtr());
    cJSON_Delete(root);
    return EXIT_FAILURE;
  }

  printf("Movie rating: %.1f\n", obj->valuedouble);

  cJSON_Delete(root);
  return EXIT_SUCCESS;
}
