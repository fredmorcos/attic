// Build using:
//
//   cc -Wall -Wextra -Ltarget/debug -Iinclude -lmylib tests/test_auto.c -o
//   tests/test_auto
//
//   cc -Wall -Wextra -Ltarget/release -Iinclude -lmylib tests/test_auto.c -o
//   tests/test_auto
//
// Running:
//
//   LD_LIBRARY_PATH=target/debug ./tests/test_auto
//
//   LD_LIBRARY_PATH=target/release ./tests/test_auto
//
// With Valgrind (to show that really memory has been freed properly):
//
//   LD_LIBRARY_PATH=target/debug valgrind ./tests/test_auto    # Very slow
//
//   LD_LIBRARY_PATH=target/release valgrind ./tests/test_auto

#include <auto_pq.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  uint8_t elements[5] = {4, 2, 5, 1, 2};
  struct Pq *pq = pq_create(elements, 5);

  if (pq == NULL) {
    printf("Failed to create priority queue\n");
    exit(EXIT_FAILURE);
  }

  if (pq_push(pq, 6) == false) {
    printf("Failed to push to priority queue\n");
  }

  for (int i = 0; i < 10; i++) {
    uint8_t result = 0;
    if (pq_pop(pq, &result) == true) {
      printf("pop() = %d\n", result);
    } else {
      printf("pop() = Nothing (or invalid arguments)\n");
    }
  }

  pq_free(pq);
}
