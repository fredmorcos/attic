#pragma once

#include <stdbool.h>
#include <time.h>
#include "util.h"

bool xfstat(const char *const path, time_t *const mtime, ll *const size);
bool xlstatlen(const char *const path,
               size_t *const target_len,
               const char **const err_msg,
               char **const err_path);
bool xlstat(const char *const path,
            char *const t,
            const size_t tlen,
            const char **const err_msg,
            char **const err_path);
