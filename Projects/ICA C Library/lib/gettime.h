#ifndef GETTIME_H
#define GETTIME_H

#include <sys/time.h>
#include <time.h>

typedef struct timeval timeval_t;
typedef struct timespec timespec_t;

void get_time_usec(timeval_t *tv);
void get_time_nsec(timespec_t *tp);
double time_diff_usec(timeval_t *begin, timeval_t *end);
double time_diff_nsec(timespec_t *begin, timespec_t *end);

#endif /* GETTIME_H */
