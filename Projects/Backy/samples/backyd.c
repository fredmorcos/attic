#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <time.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <dirent.h>
#include <getopt.h>
#include <unistd.h>
#include <pthread.h>
#include <err.h>
#include "vec.h"
#include "util.h"
#include "dir.h"

int main(const int argc, char *const *const argv) {
  bool  detach = false;
  bool  safe   = false;
  bool  keep   = false;
  char *dname  = NULL;
  char *addr   = NULL;
  char *port   = NULL;

  if (argc <= 1)
    errx(EX_USAGE, "No arguments given, see -h for help");

  for (int arg = 0; (arg = getopt(argc, argv, ":hsdkr:a:p:")) != -1;) {
    if (arg == 'h') {
      warnx("usage: %s [-h -d -s -k] -r DIR -a ADDR -p PORT", argv[0]);
      warnx("%s", "");
      warnx("options:");
      warnx(" -h           Show this help");
      warnx(" -d           Detach/daemonize from terminal (fork)");
      warnx(" -s           Do not modify files and dirs (dry run)");
      warnx(" -k           Keep the daemon running after sync");
      warnx(" -r DIR       Sync onto DIR (the target directory)");
      warnx(" -p PORT      Listen on network PORT");
      warnx(" -a ADDR      Listen on network ADDR [optional]");
      return EXIT_SUCCESS;
    } else if (arg == 'd') {
      if (detach)
        warnx("Warning: Multiple -d arguments have no extra effect");
      detach = true;
    } else if (arg == 's') {
      if (safe)
        warnx("Warning: Multiple -s arguments have no extra effect");
      safe = true;
    } else if (arg == 'k') {
      if (keep)
        warnx("Warning: Multiple -k arguments have no extra effect");
      keep = true;
    } else if (arg == 'r') {
      if (dname)
        errx(EX_USAGE, "Warning: Multiple -r parameters are not allowed");
      dname = optarg;
    } else if (arg == 'a') {
      if (addr)
        errx(EX_USAGE, "Error: Multiple -a parameters are not allowed");
      addr = optarg;
    } else if (arg == 'p') {
      if (port)
        errx(EX_USAGE, "Error: Multiple -p parameters are not allowed");
      port = optarg;
    } else if (arg == ':') {
      errx(EX_USAGE, "Error: No value given for -%c parameter", optopt);
    } else if (arg == '?') {
      errx(EX_USAGE, "Error: Unrecognized argument -%c", optopt);
    } else {
      assert(false);
    }
  }

  if (!dname)
    errx(EX_USAGE, "Error: No root directory given, use -r or see -h");

  if (!port)
    errx(EX_USAGE, "Error: No network port given, use -p or see -h");

  if (detach) {
    pid_t pid = -1;

    switch ((pid = fork())) {
    case -1: err(EX_OSERR, "Error: Cannot detach (fork) process");
    case  0: break;                     /* child  */
    default:                            /* parent */
      warnx("Forked child with pid %d", pid);
      return EX_OK;
    }
  }

  /* filelist buffer */ {
    clean(vec_free) struct vec flbuf;

    vec_init(&flbuf, 2048, sizeof(char), NULL);

    /* dir loader */ {
      char *p = NULL;
      clean(dir_free) struct dir d;

      dir_init(&d, dname, &flbuf);
      tinow(&d.ti, true);

      if (!dir_load_a(&d)) {
        warn("Could not start thread to load directory %s", dname);
        warnx(" -> Will try to load directory synchronously");

        if (!dir_load(&d, dname)) {
          warn("Error: Cannot load directory %s", dname);
          dir_perr(&d);
          return EX_OSERR;
        }
      }

      if (d.thread_valid && (errno = pthread_join(d.thread, NULL)) != 0) {
        warn("Error: Could not join thread to load directory %s", dname);
        return EX_OSERR;
      }

      atomic_flag_clear(&d.run);
      d.thread_valid = false;

      if (!(p = vec_addn(&flbuf, 1))) {
        warn("Error: Cannot allocate space for dir info");
        return EX_OSERR;
      }

      *p = NIL;

      /* print warnings and stats */
      tidiff(&d.ti);
      fswarn_printl(&d.warns);
      iopbufstat(d.nents, flbuf.len, &d.ti);
    }
  }

  return EX_OK;
}
