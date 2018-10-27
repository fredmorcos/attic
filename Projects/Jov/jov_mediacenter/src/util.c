#include "util.h"

#include <SDL.h>
#include <time.h>
#include <errno.h>

SDL_bool util_time_date (char *time_text, size_t time_max_len,
                         char *date_text, size_t date_max_len) {
  time_t t;
  struct tm *to = NULL;

  if ((t = time(NULL)) == ((time_t) -1)) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Cannot get time since UNIX epoch: %s\n", strerror(errno));
    return SDL_FALSE;
  }

  if ((to = localtime(&t)) == NULL) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Error getting time (locatime())\n");
    return SDL_FALSE;
  }

  if (strftime(time_text, time_max_len, "%T", to) == 0) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Error formatting time (%s)\n", "strftime()");
    return SDL_FALSE;
  }

  if (strftime(date_text, date_max_len, "%A, %d %B %Y", to) == 0) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                 "Error formatting date (%s)\n", "strftime()");
    return SDL_FALSE;
  }

  return SDL_TRUE;
}
