#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <bsd/bsd.h>

_Thread_local int x = 4;

void pthread_destr(pthread_t *t) {
  warnx("Thread Destructor: Will try to cancel thread");
  pthread_cancel(*t);
  pthread_join(*t, NULL);
  warnx("Thread Destructor: Thread finished cleanly");
  warnx("Main: x = %d", x);
}

void *t_func(void *something) {
  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);
  x = 2;
  warnx("Thread: x = %d", x);
  while (1) {
    warnx("Thread: Will sleep for 2 seconds");
    sleep(2);
    pthread_testcancel();
    continue;
  }
  return NULL;
}

int main (void) {
  __attribute__((cleanup(pthread_destr))) pthread_t t;

  x = 6;
  warnx("Main: x = %d", x);

  pthread_create(&t, NULL, t_func, NULL);

  warnx("Will sleep for 5 seconds");
  sleep(5);

  warnx("Main: x = %d", x);
  /*
   * warnx("Will try to cancel thread");
   * pthread_cancel(t);
   * pthread_join(t, NULL);
   * warnx("Thread finished cleanly");
   */

  pthread_destr(&t);

  warnx("Main: x = %d", x);

  return 0;
}
