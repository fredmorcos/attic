#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include "thread.h"

void mutex_destroy(pthread_mutex_t *const mutex) {
  int _errno = errno;
  (void) pthread_mutex_destroy(mutex);
  errno = _errno;
}

bool thread_init(struct thread *const self,
                 thread_cb func,
                 void *const arg) {
  assert(self != NULL);
  assert(func != NULL);

  self->thread_valid = false;
  self->cancel = false;

  self->func = func;
  self->arg = arg;

  if ((errno = pthread_mutex_init(&self->cancel_mutex, NULL)) != 0)
    return (false);

  if ((errno = pthread_create(&self->thread, NULL, func, arg)) != 0) {
    assert(errno != EINVAL);
    mutex_destroy(&self->cancel_mutex);
    return (false);
  }

  self->thread_valid = true;

  return (true);
}

void thread_cancel(struct thread *const self) {
  assert(self != NULL);

  if (!self->thread_valid)
    return;

  if ((errno = pthread_mutex_lock(&self->cancel_mutex)) != 0)
    assert(errno != EDEADLK);

  self->cancel = true;

  if ((errno = pthread_mutex_unlock(&self->cancel_mutex)) != 0)
    assert(errno != EPERM);

  thread_waitout(self);
}

void thread_cancelp(struct thread **const self) {
  assert(self != NULL);
  thread_cancel(*self);
}

void thread_waitout(struct thread *const self) {
  assert(self != NULL);

  /* silence the warn_unused_result warning */
  if (self->thread_valid && thread_result(self))
    return;
  else
    return;
}

void *thread_result(struct thread *const self) {
  assert(self != NULL);

  if (self->thread_valid) {
    void *ret = NULL;

    if ((errno = pthread_join(self->thread, &ret)) != 0) {
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
