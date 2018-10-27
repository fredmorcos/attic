#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main (void) {
  struct winsize ws;

  while (1) {
    ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
    printf("WIDTH = %d    HEIGHT = %d\n", ws.ws_col, ws.ws_row);
    sleep(2);
  }

  return 0;
}

