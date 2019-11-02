#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *readline();

char *isBalanced(char *s) {
  char q[1000];
  int ql = 0;

  while (*s) {
    if (*s == '{' || *s == '[' || *s == '(') {
      q[ql++] = *s; /* Push open brackets */
    } else if (*s == '}' || *s == ']' || *s == ')') {
      if (ql == 0) {
        return "NO";
      } else if (*s == '}' && q[ql - 1] == '{') {
        ql--;
      } else if (*s == ']' && q[ql - 1] == '[') {
        ql--;
      } else if (*s == ')' && q[ql - 1] == '(') {
        ql--;
      } else {
        return "NO";
      }
    }
    s++;
  }

  if (ql == 0) {
    return "YES";
  }

  return "NO";
}

char *isBalanced_bad(char *s) {
  if (*s == 0) {
    return "YES"; /* Empty string is balanced */
  } else if (*s == '}' || *s == ']' || *s == ')') {
    return "NO";
  }

  char q[1000]; /* Q */
  int ql = 0;   /* Length of Q */

  q[ql++] = *s; /* Push opening bracket */
  s++;          /* Next char */

  while (*s) {
    if (*s == '{' || *s == '[' || *s == '(') {
      q[ql++] = *s; /* Push open brackets */
      s++;
      continue;
    }

    /* We can assume that the current char is a closing bracket, if Q
       is empty then the list is unbalanced */
    if (ql == 0) {
      return "NO";
    }

    /* If the last element in Q is not a corresponding opening bracket
       to the closing one we have, then the list is unbalanced */
    if ((*s == '}' && q[ql - 1] != '{') || (*s == ']' && q[ql - 1] != '[') ||
        (*s == ')' && q[ql - 1] != '(')) {
      return "NO";
    }

    ql--; /* Pop opening bracket */
    s++;  /* Next char */
  }

  if (ql == 0) {
    return "YES";
  }

  return "NO";
}

int main() {
  FILE *fptr = fopen(getenv("OUTPUT_PATH"), "w");

  char *t_endptr;
  char *t_str = readline();
  int t = strtol(t_str, &t_endptr, 10);

  if (t_endptr == t_str || *t_endptr != '\0') {
    exit(EXIT_FAILURE);
  }

  for (int t_itr = 0; t_itr < t; t_itr++) {
    char *s = readline();

    char *result = isBalanced(s);

    fprintf(fptr, "%s\n", result);
  }

  fclose(fptr);

  return 0;
}

char *readline() {
  size_t alloc_length = 1024;
  size_t data_length = 0;
  char *data = malloc(alloc_length);

  while (true) {
    char *cursor = data + data_length;
    char *line = fgets(cursor, alloc_length - data_length, stdin);

    if (!line) {
      break;
    }

    data_length += strlen(cursor);

    if (data_length < alloc_length - 1 || data[data_length - 1] == '\n') {
      break;
    }

    size_t new_length = alloc_length << 1;
    data = realloc(data, new_length);

    if (!data) {
      break;
    }

    alloc_length = new_length;
  }

  if (data[data_length - 1] == '\n') {
    data[data_length - 1] = '\0';
  }

  data = realloc(data, data_length);

  return data;
}
