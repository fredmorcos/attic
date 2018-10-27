#include <stdio.h>
#include <stdlib.h>

int main (int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "Error: too little arguments\n");
    fprintf(stderr, " -> Try: %s help\n", argv[0]);

    return EXIT_FAILURE;
  }

  

  return EXIT_SUCCESS;
}
