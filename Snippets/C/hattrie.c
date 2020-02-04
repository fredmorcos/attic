#include <stdio.h>
#include <string.h>
#include <hat-trie/hat-trie.h>

int main () {
  hattrie_t *table = hattrie_create();

  char *k = "hello";

  value_t *v = hattrie_get(table, k, strlen(k));

  if (v == NULL) {
    printf("v is NULL\n");
  } else {
    printf("v is not NULL: %lu\n", *v);
  }

  char *k2 = "hello";

  value_t *v2 = hattrie_get(table, k2, strlen(k));

  if (v2 == NULL) {
    printf("v2 is NULL\n");
  } else {
    printf("v2 is not NULL: %lu\n", *v2);
  }

  value_t *v3 = hattrie_get(table, k, strlen(k));

  if (v3 == NULL) {
    printf("v3 is NULL\n");
  } else {
    printf("v3 is not NULL: %lu\n", *v3);
  }

  *v3 += 10;

  value_t *v4 = hattrie_get(table, k, strlen(k));

  if (v4 == NULL) {
    printf("v4 is NULL\n");
  } else {
    printf("v4 is not NULL: %lu\n", *v4);
  }

  hattrie_free(table);

  return 0;
}
