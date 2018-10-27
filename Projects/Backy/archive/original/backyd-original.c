#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sysexits.h>
#include <bsd/bsd.h>
#include "array.h"
#include "dir.h"
#include "dir-print.h"
#include "dir-parse.h"
#include "dir-diff.h"
#include "net.h"
#include "util.h"

int main(const int argc, char **argv) {
  bool  detach  = false;
  bool  safe    = false;
  char *dirname = NULL;
  char *port    = NULL;

  int sfd = 0;
  int cfd = 0;

  clock_t dir_print_cl;
  clock_t gen_cl;

  char   *dirbuf = NULL;
  size_t  dirbuf_len = 0;

  struct dir_print_params dir_print_p = {
    .path = ".",
    .len = &dirbuf_len,
    .cflag = false,
    .is_action_list = false
  };

  struct array actionsbuf;
  struct array actions;

  array_init(&actionsbuf, sizeof(char), NULL);
  array_init(&actions, sizeof(struct dir), NULL);

  if (argc <= 1)
    errx(EX_USAGE, "No arguments given, see -h for help");

  for (int arg = 0; (arg = getopt(argc, argv, ":hsdr:p:")) != -1;) {
    if (arg == 'h') {
      warnx("usage: %s [-h -d] -r DIR -p PORT", getprogname());
      warnx("%s", "");
      warnx("options:");
      warnx(" -h           Show this help");
      warnx(" -d           Detach/daemonize");
      warnx(" -s           Do not modify files and dirs (dry run)");
      warnx(" -r DIR       Sync onto DIR");
      warnx(" -p PORT      Listen on PORT");
      return 0;
    } else if (arg == 'd') {
      if (detach)
        warnx("Multiple -d options have no extra effect");
      detach = true;
    } else if (arg == 's') {
      if (safe)
        warnx("Multiple -s options have no extra effect");
      safe = true;
    } else if (arg == 'r') {
      if (dirname)
        errx(EX_USAGE, "Multiple -r arguments are not allowed");
      dirname = optarg;
    } else if (arg == 'p') {
      if (port)
        errx(EX_USAGE, "Multiple -p arguments are not allowed");
      port = optarg;
    } else if (arg == ':') {
      errx(EX_USAGE, "No parameter given for -%c", optopt);
    } else if (arg == '?') {
      errx(EX_USAGE, "Unrecognized argument -%c", optopt);
    }
  }

  if (!dirname)
    errx(EX_USAGE, "No root directory given, use -r");

  if (!port)
    errx(EX_USAGE, "No port given, use -p");

  if (chdir(dirname) == -1)
    err(EX_CANTCREAT, "chdir(): %s", dirname);

  if (detach) {
    pid_t pid = -1;

    switch((pid = fork())) {
    case -1: err(EX_OSERR, "fork()");
    case  0: break;                     /* child */
    default:                            /* parent */
      warnx("Forked child PID: %d", pid);
      return 0;
    }
  }

  dir_print_cl = clock();
  if (pthread_create(&dir_print_p.thread, NULL,
                     (thread_cb) dir_print_thread,
                     &dir_print_p) != 0) {
    warnx("Cannot create thread to load dir structure");
    return EX_OSERR;
  }

  if ((sfd = socket_listen(port)) == -1) {
    dir_print_p.cflag = true;
    (void) pthread_join(dir_print_p.thread, (void **) &dirbuf);
    if (dirbuf) free(dirbuf);
    return EX_OSERR;
  }

  if ((cfd = socket_accept(sfd)) == -1) {
    socket_close(cfd, "listening socket");
    dir_print_p.cflag = true;
    (void) pthread_join(dir_print_p.thread, (void **) &dirbuf);
    if (dirbuf) free(dirbuf);
    return EX_OSERR;
  }

  (void) pthread_join(dir_print_p.thread, (void **) &dirbuf);
  if (!dirbuf) {
    warnx("Could not get dir structure buffer: %s", dirname);
    socket_close(cfd, "connection socket");
    return EX_OSERR;
  }
  data_stats("Loaded dir structure", timediff(dir_print_cl), dirbuf_len);

  gen_cl = clock();
  if (socket_sendbuf(cfd, dirbuf, dirbuf_len) == -1) {
    warn("send()");
    socket_close(cfd, "connection socket");
    free(dirbuf);
    return EX_OSERR;
  }
  data_stats("Sent dir structure", timediff(gen_cl), dirbuf_len);

  /* We don't need that anymore. */
  free(dirbuf);

  gen_cl = clock();
  if (socket_recvbuf(cfd, 1024 * 1024, &actionsbuf) == -1) {
    array_free(&actionsbuf);
    socket_close(cfd, "connection socket");
    return EX_OSERR;
  }
  data_stats("Received dir-diff action list", timediff(gen_cl), actionsbuf.len);

  if (dir_parsebuf(true, actionsbuf.ptr, &actions) == -1) {
    warnx("Could not parse dir-diff action list");
    socket_close(cfd, "connection socket");
    array_free(&actionsbuf);
    return EX_OSERR;
  }

  if (actions.len == 0) {
    assert(actions.alen == 0);
    assert(actions.ptr == NULL);

    warnx("Action list is empty, nothing to be done.");
    array_free(&actions);
    array_free(&actionsbuf);
    socket_close(cfd, "connection socket");
    return 0;
  }

  warnx("Action list has %zd elements", actions.len);
  array_traverse1(&actions, (array_cb1) dir_diff_action, &safe);

  array_free(&actions);
  array_free(&actionsbuf);
  socket_close(cfd, "connection socket");

  return 0;
}
