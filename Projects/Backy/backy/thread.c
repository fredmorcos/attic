#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include "thread.h"

void mutex_destroy(Mutex *const mutex) {
  int _errno = errno;
  (void) pthread_mutex_destroy(mutex);
  errno = _errno;
}

bool thread_init(Thread *const self,
                 ThreadCB func,
                 void *const arg) {
  assert(self != NULL);
  assert(func != NULL);

  self->thread_valid = false;
  self->cancel = false;

  self->func = func;
  self->arg = arg;

  errno = pthread_mutex_init
    (&self->cancel_mutex, NULL);

  if (errno != 0)
    return (false);

  errno = pthread_create
    (&self->thread, NULL, func, arg);

  if (errno != 0) {
    assert(errno != EINVAL);
    mutex_destroy(&self->cancel_mutex);
    return (false);
  }

  self->thread_valid = true;

  return (true);
}

void thread_cancel(Thread *const self) {
  assert(self != NULL);

  if (!self->thread_valid)
    return;

  errno = pthread_mutex_lock
    (&self->cancel_mutex);

  if (errno != 0)
    assert(errno != EDEADLK);

  self->cancel = true;

  errno = pthread_mutex_unlock
    (&self->cancel_mutex);

  if (errno != 0)
    assert(errno != EPERM);

  thread_waitout(self);
}

void thread_cancelp(Thread **const self) {
  assert(self != NULL);
  thread_cancel(*self);
}

void thread_waitout(Thread *const self) {
  assert(self != NULL);

  /* silence the warn_unused_result warning */
  if (self->thread_valid && thread_result(self))
    return;
  else
    return;
}

void *thread_result(Thread *const self) {
  assert(self != NULL);

  if (self->thread_valid) {
    void *ret = NULL;

    errno = pthread_join
      (self->thread, &ret);

    if (errno != 0) {
      assert(errno != EDEADLK);
      assert(errno != EINVAL);
      assert(errno != ESRCH);
      return (NULL);
    }

    mutex_destroy(&self->cancel_mutex);
    self->thread_valid = false;

    return (ret);
  } else {
    return (self->func(self->arg));
  }
}
