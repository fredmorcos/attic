#include <assert.h>
#include <stdbool.h>
#include <errno.h>
#include <pthread.h>
#include "thread.h"

typedef void *(*thread_run_cb)(void *const);

static void *thread_run_helper(struct thread *const self)
  attr_nonnull_all;

static void thread_cancel_en(void);
static void thread_cancel_dis(void);
static void thread_setcancel(void);

bool thread_init(struct thread *const self,
                 thread_cb func,
                 void *const data) {
  assert(self != NULL);
  assert(func != NULL);

  if ((errno = pthread_mutex_init(&self->cancel_mutex, NULL)) != 0)
    return (self->cancel_mutex_valid = false);
  self->cancel_mutex_valid = true;

  self->cancel = false;
  self->valid = false;
  self->func = func;
  self->data = data;

  return true;
}

bool thread_run(struct thread *const self) {
  assert(self != NULL);
  assert(self->func != NULL);
  assert(self->cancel == false);
  assert(self->valid == false);

  errno = pthread_create(&self->thread,
                         NULL,
                         (thread_run_cb) thread_run_helper,
                         self);
  return (self->valid = (errno == 0));
}

static void *thread_run_helper(struct thread *const self) {
  assert(self != NULL);
  assert(self->func != NULL);

  bool ret;

  thread_setcancel();
  thread_cancel_dis();

  ti_now(&self->ti, true);
  ret = self->func(self->data, self);
  ti_diff(&self->ti);

  thread_cancel_en();

  return ret == true ? self->data : NULL;
}

void *thread_wait(struct thread *const self) {
  assert(self != NULL);

  void *retval = NULL;

  if (self->valid) {
    if ((errno = pthread_cancel(self->thread)) != 0)
      return NULL;

    if ((errno = pthread_join(self->thread, &retval)) != 0)
      return NULL;
  }
}

void thread_destroy(struct thread *const self) {
  assert(self != NULL);

  if (self->valid) {
    (void) pthread_cancel(self->thread);
    (void) pthread_join(self->thread, NULL);
  }

  if (self->cancel_mutex_valid == true)
    (void) pthread_mutex_destroy(&self->cancel_mutex);
}

static void thread_cancel_en(void) {
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
}

static void thread_cancel_dis(void) {
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
}

static void thread_setcancel(void) {
  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);
}
