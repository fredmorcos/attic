#include <stdbool.h>
#include <limits.h>
#include <pthread.h>
#include <sysexits.h>
#include <bsd/bsd.h>
#include "array.h"
#include "dir.h"
#include "dir-get.h"
#include "dir-diff.h"
#include "dir-print.h"
#include "dir-parse.h"
#include "net.h"
#include "util.h"

int main(const int argc, char **argv) {
  char *dirname = NULL;
  char *host    = NULL;
  char *port    = NULL;

  int cfd = 0;

  clock_t dir_get_cl;
  clock_t gen_cl;

  struct array cdirs;
  struct array sdirs;
  struct array sdirs_buf;
  struct array actions;

  struct dir_get_params dir_get_p = {
    .path  = ".",
    .res   = &cdirs,
    .cflag = false
  };

  char   *actionbuf = NULL;
  size_t  actionbuf_len = 0;

  array_init(&cdirs, sizeof(struct dir), (array_cb) dir_free);
  array_init(&sdirs, sizeof(struct dir), NULL);
  array_init(&sdirs_buf, sizeof(char), NULL);

  if (argc <= 1)
    errx(EX_USAGE, "No arguments given, see -h for help");

  for (int arg = 0; (arg = getopt(argc, argv, ":hr:a:p:")) != -1;) {
    if (arg == 'h') {
      warnx("usage: %s [-h] -r DIR -i HOST -p PORT", getprogname());
      warnx("%s", "");
      warnx("options:");
      warnx(" -h           Show this help");
      warnx(" -r DIR       Sync onto DIR");
      warnx(" -a HOST      Connect to HOST");
      warnx(" -p PORT      Connect on PORT");
      return 0;
    } else if (arg == 'r') {
      if (dirname)
        errx(EX_USAGE, "Multiple -r arguments are not allowed");
      dirname = optarg;
    } else if (arg == 'a') {
      if (host)
        errx(EX_USAGE, "Multiple -a arguments are not allowed");
      host = optarg;
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
    errx(EX_USAGE, "No root dir given, use -r");

  if (!host)
    errx(EX_USAGE, "No host given, use -i");

  if (!port)
    errx(EX_USAGE, "No port given, use -p");

  if (chdir(dirname) == -1)
    err(EX_CANTCREAT, "chdir(): %s", dirname);

  dir_get_cl = clock();
  if (pthread_create(&dir_get_p.thread, NULL,
                     (thread_cb) dir_get_thread,
                     &dir_get_p) != 0) {
    warnx("Cannot create thread to load dir structure");
    return EX_OSERR;
  }

  if ((cfd = socket_connect(host, port)) == -1) {
    dir_get_p.cflag = true;
    (void) pthread_join(dir_get_p.thread, NULL);
    array_free(&cdirs);
    return EX_OSERR;
  }

  gen_cl = clock();
  if (socket_recvbuf(cfd, 2 * 1024 * 1024, &sdirs_buf) == -1) {
    socket_close(cfd, "client socket");
    array_free(&sdirs_buf);
    dir_get_p.cflag = true;
    (void) pthread_join(dir_get_p.thread, NULL);
    array_free(&cdirs);
    return EX_OSERR;
  }
  data_stats("Received server dir structure", timediff(gen_cl), sdirs_buf.len);

  gen_cl = clock();
  if (dir_parsebuf(false, sdirs_buf.ptr, &sdirs) == -1) {
    socket_close(cfd, "client socket");
    array_free(&sdirs);
    array_free(&sdirs_buf);
    dir_get_p.cflag = true;
    (void) pthread_join(dir_get_p.thread, NULL);
    array_free(&cdirs);
    return EX_OSERR;
  }
  element_stats("Loaded server dir structure", timediff(gen_cl), sdirs.len);

  (void) pthread_join(dir_get_p.thread, NULL);
  if (dir_get_p.ret != 0) {
    warnx("Could not get dir structure: %s", dirname);
    socket_close(cfd, "client socket");
    array_free(&cdirs);
    array_free(&sdirs);
    array_free(&sdirs_buf);
    return EX_OSERR;
  }
  element_stats("Loaded client dir structure", timediff(dir_get_cl), cdirs.len);

  gen_cl = clock();
  if (dir_diff(&cdirs, &sdirs, &actions) == -1) {
    warnx("Could not process dir-diff");
    socket_close(cfd, "client socket");
    array_free(&cdirs);
    array_free(&sdirs);
    array_free(&sdirs_buf);
    return EX_OSERR;
  }
  element_stats("Calculated diff (worst-case)", timediff(gen_cl),
                cdirs.len == 0 ? sdirs.len :
                sdirs.len == 0 ? cdirs.len :
                cdirs.len * sdirs.len);

  warnx("Action list has %zd elements", actions.len);
  array_traverse(&actions, (array_cb) dir_diff_print);

  if (!(actionbuf = dir_sprint(true, &actions, &actionbuf_len))) {
    warnx("Could not create buffer for dir-diff actions list");
    socket_close(cfd, "client socket");
    array_free(&actions);
    array_free(&cdirs);
    array_free(&sdirs);
    array_free(&sdirs_buf);
    return EX_OSERR;
  }

  /* We don't need that anymore. */
  array_free(&actions);
  array_free(&cdirs);
  array_free(&sdirs);
  array_free(&sdirs_buf);

  gen_cl = clock();
  if (socket_sendbuf(cfd, actionbuf, actionbuf_len) == -1) {
    warn("send()");
    socket_close(cfd, "client socket");
    free(actionbuf);
    return EX_OSERR;
  }
  data_stats("Sent action list", timediff(gen_cl), actionbuf_len);

  free(actionbuf);
  socket_close(cfd, "client socket");

  return 0;
}
