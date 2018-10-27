#if !defined(BK_NET)
#define BK_NET
BK_NET

#include <netdb.h>

#define NET_ESOCKET  (-1)
#define NET_EBIND    (-2)
#define NET_ELISTEN  (-3)
#define NET_ECONNECT (-4)

char *hostname(void);
char *nameinfo(const struct sockaddr *const addr,
               const socklen_t alen,
               int *const ret);

int tcp_addrinfo(const int fam,
                 const char *const host,
                 const char *const port,
                 struct addrinfo **const ai);
int tcp_listen(const struct addrinfo *const ai);
int tcp_connect(const struct addrinfo *const ai);
int tcp_retryaccept(const int _errno);
const char *tcp_error(const int rv);

void sock_enopt(const int fd, const int optname);
int sock_close(const int fd, const int rv);

const char *addrfam(const int fam);
int addrstrlen(const int fam);
const char *addrip(const int fam, const struct sockaddr *const addr);

void ai_warn(const int errval, const char *const msg);
void ai_err(const int errval, const int ecode, const char *const msg);
void ai_perr(const char *const prefix,
             const struct addrinfo *const ai,
             const char *const port,
             const char *const msg);
void ai_pmsg(const char *const prefix,
             const struct addrinfo *const ai,
             const char *const port);
void sa_pmsg(const char *const prefix,
             const int fam,
             const struct sockaddr *const sa);

#endif
