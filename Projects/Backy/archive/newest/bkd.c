#include <assert.h>
#include <err.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>
#include <getopt.h>
#include <arpa/inet.h>
#include "fs.h"
#include "mem.h"
#include "num.h"
#include "stat.h"
#include "str.h"
#include "time.h"
#include "vec.h"
#include "lsdir.h"
#include "ai.h"
#include "net.h"

int main(int argc, char *argv[]) {
  bool detach = false;
  bool safe   = false;
  bool keep   = false;
  bool ipv4   = false;
  bool ipv6   = false;

  char *rootdir = NULL;
  char *host    = NULL;
  char *port    = NULL;

  int net_fam = AF_UNSPEC;

  /* socket fds */
  __attribute__((cleanup(sock_close))) int lfd = -1;
  __attribute__((cleanup(sock_close))) int cfd = -1;

  if (argc <= 1) {
  usage:
    errx(EX_USAGE, "usage: bkd [-46dsk] -r DIR [-a HOST] -p PORT");
  }

  for (int ch = 0; (ch = getopt(argc, argv, ":46dskr:a:p:")) != -1;) {
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
    case 'd':
      if (detach) warnx("multiple -d flags have no extra effect");
      detach = true;
      break;
    case 's':
      if (safe) warnx("multiple -s flags have no extra effect");
      safe = true;
      break;
    case 'k':
      if (keep) warnx("multiple -k flags have no extra effect");
      keep = true;
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

  if (!rootdir || !port)
    goto usage;

  assert(!(ipv4 && ipv6));

  net_fam = ipv4 ? AF_INET : ipv6 ? AF_INET6 : AF_UNSPEC;

  {                                     /* print local hostname */
    char hostname[HOST_NAME_MAX + 1];

    if (gethostname(hostname, sizeof(hostname)) == -1)
      warn("cannot get local hostname");
    else
      warnx("hostname: %s", hostname);
  }

  {                                     /* TCP listen */
    __attribute__((cleanup(ai_free)))
      struct addrinfo *ai = NULL;
    const struct addrinfo *p = NULL;

    int gai_err = xgetai(host, port, net_fam, SOCK_STREAM, AI_PASSIVE, &ai);

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
      if ((lfd = xbindlisten(p)) == -1)
        ai_print("skip", p->ai_family, p->ai_addr, port, true);
      else
        break;
    }

    assert((lfd == -1 && p == NULL) || (lfd != -1 && p != NULL));

    if (lfd == -1) {
      const char *const _host = host == NULL ? "*" : host;
      warnx("cannot listen on port %s (on host %s)", port, _host);
      return (EX_OSERR);
    }

    ai_print("listen on", p->ai_family, p->ai_addr, port, false);
  }

  if (detach) {                         /* detach process */
    pid_t pid = fork();

    switch (pid) {
    case -1: err(EX_OSERR, "cannot detach/fork process");
    case  0: break;                     /* child */
    default:                            /* parent */
      warnx("forked child %d", pid);
      return (EX_OK);
    }
  }

  {                                     /* start loading directory */
    __attribute__((cleanup(vec_free)))
      struct vec dlist;
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

    vec_init(&dlist, "dirs_buffer", sizeof(char), 2048, NULL);

    if (thread_init(lsdir_thread, (thread_cb) lsdir, &lsdir_info))
      warnx("started loading %s", rootdir);

    {                                   /* accept TCP connection */
      struct sockaddr_storage addr;
      socklen_t addrlen = 0;

      int gni_err = 0;
      char chn[NI_MAXHOST + 1];

    retry:
      addrlen = sizeof(struct sockaddr_storage);

      warnx("waiting to accept connection...");

      if ((cfd = accept(lfd, (struct sockaddr *) &addr, &addrlen)) == -1 &&
          (errno == ENETDOWN   || errno == ENOPROTOOPT  || errno == EPROTO ||
           errno == EHOSTDOWN  || errno == EHOSTUNREACH || errno == ENONET ||
           errno == EOPNOTSUPP || errno == ENETUNREACH)) {
        warnx("%s, trying again...", strerror(errno));
        goto retry;
      } else if (cfd == -1) {
        warn("could not accept connection");
        return (EX_NOHOST);
      }

      ai_print("accepted connection from", addr.ss_family,
               (struct sockaddr *) &addr, "*", false);

      sock_close(&lfd);
      lfd = -1;

      addrlen = sizeof(struct sockaddr_storage);

      if ((gni_err = getnameinfo((struct sockaddr *) &addr, addrlen,
                                 chn, sizeof(chn), NULL, 0,
                                 NI_NAMEREQD)) != 0) {
        assert(gni_err != EAI_BADFLAGS);
        assert(gni_err != EAI_FAMILY);
        assert(gni_err != EAI_OVERFLOW);

        warnx("cannot get client hostname: %s", gai_strerror(gni_err));
      } else {
        warnx("client hostname: %s", chn);
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

    {
      __attribute__((cleanup(char_free)))
        char *hsize = humansize(dlist.len);

      if (hsize != NULL) {
        warnx("loaded %zu entries (%s) in " TI_FMT,
              lsdir_info.nents, hsize, TI_FMT_PARAMS(lsdir_info.ti));
      } else {
        warnx("loaded %zu entries (%zu bytes) in " TI_FMT,
              lsdir_info.nents, dlist.len, TI_FMT_PARAMS(lsdir_info.ti));
      }
    }

    warnx("sending directory listing...");

    if (!netsend(cfd, lsdir_info.res->ptr, lsdir_info.res->len)) {
      warn("error sending directory listing");
      return (EX_OSERR);
    }
  }

  return (EX_OK);
}
