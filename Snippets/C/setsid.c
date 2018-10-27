#include <unistd.h>
#include <err.h>

int main (void) {
  pid_t sid;

  /*
   * if (fork() != 0)
   *   return 0;
   */

  if ((sid = setsid()) == -1) {
    warn("setsid()");
  } else {
    warnx("Success sid = %d... sleeping for 3 seconds", sid);
    sleep(3);
  }

  warnx("Exiting");

  return 0;
}
