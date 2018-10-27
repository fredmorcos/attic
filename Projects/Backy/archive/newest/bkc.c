#include <assert.h>
#include <err.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sysexits.h>
#include <getopt.h>
#include <arpa/inet.h>
#include "net.h"
#include "mem.h"
#include "time.h"
#include "lsdir.h"
#include "ai.h"
#include "str.h"
#include "parse.h"

int main(int argc, char *argv[]) {
  bool ipv4 = false;
  bool ipv6 = false;

  char *rootdir = NULL;
  char *host    = NULL;
  char *port    = NULL;

  int net_fam = AF_UNSPEC;

  __attribute__((cleanup(sock_close))) int cfd = -1;

  if (argc <= 1) {
  usage:
    errx(EX_USAGE, "usage: bkc [-46] -r DIR -a HOST -p PORT");
  }

  for (int ch = 0; (ch = getopt(argc, argv, ":46r:a:p:")) != -1;) {
    switch (ch) {
    case '4':
      if (ipv4) warnx("multiple -4 flags have no extra effect");
      if (ipv6) errx(EX_USAGE, "flags -4 and -6 are incompatible");
      ipv4 = true;
      break;
    case '6':
      if (ipv6) warnx("multiple -6 flags have no extra effect");
      if (ipv4) errx(EX_USAGE, "flags -6 and -4 are incompatible");
      ipv6 = true;
      break;
    case 'r':
      if (rootdir) errx(EX_USAGE, "multiple -r arguments are not supported");
      rootdir = optarg;
      break;
    case 'a':
      if (host) errx(EX_USAGE, "multiple -a arguments are not supported");
      host = optarg;
      break;
    case 'p':
      if (port) errx(EX_USAGE, "multiple -p arguments are not supported");
      port = optarg;
      break;
    case 'h': default: goto usage;
    }
  }

  if (!rootdir || !host || !port)
    goto usage;

  assert(!(ipv4 && ipv6));

  net_fam = ipv4 ? AF_INET : ipv6 ? AF_INET6 : AF_UNSPEC;

  {                                     /* start loading directory */
    __attribute__((cleanup(vec_free)))
      struct vec dlist;
    __attribute__((cleanup(vec_free)))
      struct vec slist;
    __attribute__((cleanup(char_free)))
      char *estr = NULL;

    struct lsdir lsdir_info = {
      .name = rootdir,
      .res = &dlist,
      .nents = 0,
      .estr = &estr,
      .sys_errno = 0
    };

    __attribute__((cleanup(thread_cancelp)))
      struct thread *lsdir_thread =
      (struct thread *) &lsdir_info;

    vec_init(&dlist, "dir_tree", sizeof(struct tdir), 32, (vec_cb) tdir_free);

    if (thread_init(lsdir_thread, (thread_cb) tdir, &lsdir_info))
      warnx("started loading %s", rootdir);

    {                                   /* initiate connection */
      __attribute__((cleanup(ai_free)))
        struct addrinfo *ai = NULL;
      const struct addrinfo *p = NULL;

      int gai_err = xgetai(host, port, net_fam, SOCK_STREAM, 0, &ai);

      if (gai_err != 0) {
        assert(ai == NULL);

        assert(gai_err != EAI_BADFLAGS);
        assert(gai_err != EAI_SOCKTYPE);

        const char fmt[] = "cannot get local address info";

        if (gai_err == EAI_SYSTEM)
          warn("%s for %s:%s", fmt, host, port);
        else
          warnx("%s for %s:%s: %s", fmt, host, port, gai_strerror(gai_err));

        return (EX_OSERR);
      }

      assert(ai != NULL);

      for (p = ai; p != NULL; p = p->ai_next) {
        if ((cfd = xconnect(p)) == -1)
          ai_print("skip", p->ai_family, p->ai_addr, port, true);
        else
          break;
      }

      assert((cfd == -1 && p == NULL) || (cfd != -1 && p != NULL));

      if (cfd == -1) {
        warnx("cannot connect to host %s on port %s", host, port);
        return (EX_OSERR);
      }

      ai_print("connected to", p->ai_family, p->ai_addr, port, false);
    }

    {                                   /* receive dir list */
      __attribute__((cleanup(vec_free)))
        struct vec tmp_sdlist;
      const size_t chunk = 512 * 1024;
      int recv_rc = 0;

      warnx("receiving directory listing...");

      vec_init(&tmp_sdlist, "server_dirs_buffer", sizeof(char), chunk, NULL);
      recv_rc = netrecv(cfd, chunk, &tmp_sdlist);
      assert(recv_rc == -2 || recv_rc == -1 || recv_rc == 0);

      switch (recv_rc) {
      case -2:
        warnx("server unexpectedly closed connection");
        return (EX_SOFTWARE);
      case -1:
        warn("error receiving directory listing");
        return (EX_OSERR);
      case 0:
        break;
      default:
        assert(false);
      }

      {
        __attribute__((cleanup(char_free)))
          char *hsize = humansize(tmp_sdlist.len);

        if (hsize != NULL)
          warnx("received server dir listing (%s)", hsize);
        else
          warnx("received server dir listing (%zu bytes)", tmp_sdlist.len);
      }

      {
        void *bufp = tmp_sdlist.ptr;
        int res = 0;

        vec_init(&slist,
                 "server_dir_tree",
                 sizeof(struct tdir),
                 32,
                 (vec_cb) tdir_free);

        res = lsdir_parse((char **) &tmp_sdlist.ptr, &slist);
        tmp_sdlist.ptr = bufp;

        if      (res == -1) warn("error when parsing server dir listing");
        else if (res == -2) warnx("cannot parse server dir listing");
        else if (res == -3) warn("cannot parse server dir listing");

        if (res < 0)
          return (EX_SOFTWARE);

        for (size_t i = 0; i < slist.len; i++)
          warnx("%c %s",
                (((struct tdir *) slist.ptr)[i]).type,
                (((struct tdir *) slist.ptr)[i]).filename);
      }
    }

    if (!lsdir_thread->thread_valid)
      warnx("loading %s", rootdir);

    if (thread_result(lsdir_thread) == NULL) {
      errno = lsdir_info.sys_errno;

      if (lsdir_info.sys_errno == ENOMEM ||
          lsdir_info.sys_errno == EOVERFLOW)
        warn("cannot load %s", rootdir);
      else
        warn("cannot load %s", estr);

      return (EX_OSERR);
    }

    warnx("finished loading %s", rootdir);

    warnx("loaded %zu entries in " TI_FMT,
          lsdir_info.nents, TI_FMT_PARAMS(lsdir_info.ti));
  }

  return (EX_OK);
}
