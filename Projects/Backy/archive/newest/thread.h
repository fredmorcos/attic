#pragma once

#include <stdbool.h>
#include <pthread.h>

typedef void *(*thread_cb)(void *const);

struct thread {
  pthread_t thread;
  pthread_mutex_t cancel_mutex;

  bool thread_valid;
  bool cancel;

  thread_cb func;
  void *arg;
} __attribute__((designated_init));

void mutex_destroy(pthread_mutex_t *const mutex)
  __attribute__((nonnull));

bool thread_init(struct thread *const self,
                 thread_cb func,
                 void *const arg)
  __attribute__((warn_unused_result, nonnull));

void thread_cancel(struct thread *const self)
  __attribute__((nonnull));

void thread_cancelp(struct thread **const self)
  __attribute__((nonnull));

void thread_waitout(struct thread *const self)
  __attribute__((nonnull));

void *thread_result(struct thread *const self)
  __attribute__((warn_unused_result, nonnull));
