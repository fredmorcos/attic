#ifdef __BERKELEY_UPC__
/* ensure static data in this file is made per-thread */
#pragma upc upc_code
#endif

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <inttypes.h>

#ifndef NUM_TIMERS
#define NUM_TIMERS 64
#endif

static int64_t start[NUM_TIMERS], elapsed[NUM_TIMERS];

static int64_t mygetMicrosecondTimeStamp(void) {
    int64_t retval;
    struct timeval tv;
    if (gettimeofday(&tv, NULL)) {
        perror("gettimeofday");
        abort();
    }
    retval = ((int64_t)tv.tv_sec) * 1000000 + tv.tv_usec;
    return retval;
}
#define TIME() mygetMicrosecondTimeStamp()

/*****************************************************************/
/******            T  I  M  E  R  _  C  L  E  A  R          ******/
/*****************************************************************/
void timer_clear( int n )
{
    elapsed[n] = 0.0;
}


/*****************************************************************/
/******            T  I  M  E  R  _  S  T  A  R  T          ******/
/*****************************************************************/
void timer_start( int n )
{
    start[n] = mygetMicrosecondTimeStamp();
}


/*****************************************************************/
/******            T  I  M  E  R  _  S  T  O  P             ******/
/*****************************************************************/
void timer_stop( int n )
{
    elapsed[n] += mygetMicrosecondTimeStamp() - start[n];
}


/*****************************************************************/
/******            T  I  M  E  R  _  R  E  A  D             ******/
/*****************************************************************/
double timer_read( int n )
{
    return( elapsed[n]/1000000.0 );
}

