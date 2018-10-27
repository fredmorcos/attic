#pragma once

#include <stdbool.h>
#include <pthread.h>
#include "time.h"

struct thread;
typedef bool (*thread_cb)(void *const, struct thread *const);

struct thread_mutex {
  pthread_mutex_t mutex;
  bool valid;
} attr_designated_init;

struct thread {
  pthread_t thread;
  bool valid;

  struct thread_mutex cancel_mutex;
  bool cancel;

  thread_cb func;
  void *data;

  struct time_info ti;
} attr_designated_init;

bool thread_init(struct thread *const self,
                 thread_cb *const func,
                 void *const data)
  attr_nonnull((1, 2));

bool thread_run(struct thread *const self)
  attr_nonnull_all;

void thread_destroy(struct thread *const self)
  attr_nonnull_all;
