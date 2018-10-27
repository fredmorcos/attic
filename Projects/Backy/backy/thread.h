#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <pthread.h>

typedef pthread_mutex_t Mutex;

typedef void *(*ThreadCB)(void *const);

struct thread {
  pthread_t thread;
  Mutex cancel_mutex;

  bool thread_valid;
  bool cancel;

  ThreadCB func;
  void *arg;
} attr((designated_init));

typedef struct thread Thread;

#define AutoThread \
  attr((cleanup(thread_cancelp))) Thread

void mutex_destroy(Mutex *const mutex)
  attr((nonnull));

bool thread_init(Thread *const self,
                 ThreadCB func,
                 void *const arg)
  attr((warn_unused_result, nonnull(1,2)));

void thread_cancel(Thread *const self)
  attr((nonnull));

void thread_cancelp(Thread **const self)
  attr((nonnull));

void thread_waitout(Thread *const self)
  attr((nonnull));

void *thread_result(Thread *const self)
  attr((warn_unused_result, nonnull));
