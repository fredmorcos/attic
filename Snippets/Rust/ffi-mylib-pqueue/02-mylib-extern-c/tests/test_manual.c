// Build using:
//
//   cc -Wall -Wextra -Ltarget/debug -Iinclude -lmylib tests/test_manual.c -o
//   tests/test_manual
//
//   cc -Wall -Wextra -Ltarget/release -Iinclude -lmylib tests/test_manual.c -o
//   tests/test_manual
//
// Running:
//
//   LD_LIBRARY_PATH=target/debug ./tests/test_manual
//
//   LD_LIBRARY_PATH=target/release ./tests/test_manual
//
// With Valgrind (to show that really memory has been freed properly):
//
//   LD_LIBRARY_PATH=target/debug valgrind ./tests/test_manual    # Very slow
//
//   LD_LIBRARY_PATH=target/release valgrind ./tests/test_manual

#include <manual_pq.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

int main() {
  uint8_t elements[5] = {4, 2, 5, 1, 2};
  struct PriorityQueueU8 *pq = pq_create(elements, 5);
  pq_push(pq, 6);

  for (int i = 0; i < 10; i++) {
    uint8_t result = 0;
    if (pq_pop(pq, &result) == true) {
      printf("pop() = %d\n", result);
    } else {
      printf("pop() = Nothing\n");
    }
  }

  pq_free(pq);
}
