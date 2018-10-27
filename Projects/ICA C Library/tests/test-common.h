#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include <stdio.h>

#define RUN_TEST_FUNC(f)			\
  do {						\
  printf("Running " #f "... ");			\
  if (f())					\
    printf("FAIL\n");				\
  else						\
    printf("SUCCESS\n");			\
  } while (0);

#endif /* TEST_COMMON_H */
