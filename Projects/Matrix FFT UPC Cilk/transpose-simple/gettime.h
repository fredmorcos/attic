#ifndef GETTIME_H
#define GETTIME_H

#include <time.h>

typedef struct timespec timespec_t;

void get_time_nsec(timespec_t *tp);
double time_diff_nsec(timespec_t *begin, timespec_t *end);

#endif /* GETTIME_H */
