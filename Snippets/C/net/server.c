#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

/* Networking */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

int main (int argc, __attribute__((unused)) char *argv[static argc - 1]) {
  int status;
  int sockfd;
  int sockopt = 1;

  struct addrinfo hints, *res, *p;

  void *ip_sa = NULL;
  char ip[INET6_ADDRSTRLEN];
  const char *ipver = NULL;

  struct sockaddr_storage client_addr;
  socklen_t client_addr_len;
  int client_sockfd;

  ssize_t send_status;

  memset(&hints, 0, sizeof(hints));          /* clear hints struct */

  hints.ai_family   = AF_UNSPEC;             /* ipv4 or ipv6 */
  hints.ai_socktype = SOCK_STREAM;           /* TCP */
  hints.ai_flags    = AI_PASSIVE;            /* localhost's IP addr */

  if ((status = getaddrinfo(NULL, "3490", &hints, &res)) != 0) {
    fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(status));
    exit(1);
  }

  printf("IPs:\n");

  for (p = res; p; p = p->ai_next) {
    if (p->ai_family == AF_INET) {
      ip_sa = &(((struct sockaddr_in *) p->ai_addr)->sin_addr);
      ipver = "IPv4";
    } else if (p->ai_family == AF_INET6) {
      ip_sa = &(((struct sockaddr_in6 *) p->ai_addr)->sin6_addr);
      ipver = "IPv6";
    } else {
      printf("  Unknown\n");
      continue;
    }

    inet_ntop(p->ai_family, ip_sa, ip, sizeof(ip));
    printf("  %s: %s\n", ipver, ip);
  }

  if ((sockfd = socket(res->ai_family,
                       res->ai_socktype,
                       res->ai_protocol)) == -1) {
    perror("socket(): Error creating socket");
    freeaddrinfo(res);
    exit(1);
  }

  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR,
                 &sockopt, sizeof(sockopt)) == -1) {
    perror("setsockopt(): Setting socket options");
    close(sockfd);
    freeaddrinfo(res);
    exit(1);
  }

  if (bind(sockfd, res->ai_addr, res->ai_addrlen) == -1) {
    perror("bind(): Cannot bind");
    close(sockfd);
    freeaddrinfo(res);
    exit(1);
  }

  if (listen(sockfd, 5) == -1) {
    perror("listen(): Cannot listen on socket");
    close(sockfd);
    freeaddrinfo(res);
    exit(1);
  }

  client_addr_len = sizeof(client_addr);
  if ((client_sockfd = accept(sockfd, (struct sockaddr *) &client_addr,
                              &client_addr_len)) == -1) {
    perror("accept(): Could not accept connection");
    close(sockfd);
    freeaddrinfo(res);
    exit(1);
  }

  char data[] = "Connected\n";
  send_status = send(client_sockfd, data, sizeof(data), 0);

  if (send_status == -1) {
    perror("send(): Couldn't send data");
    close(client_sockfd);
    close(sockfd);
    freeaddrinfo(res);
    exit(1);
  } else if (send_status < (ssize_t) sizeof(data)) {
    printf("Warning: Could not send all data (%zd of %zd)\n",
           send_status, sizeof(data));
  }

  if (close(client_sockfd) == -1) {
    perror("close(): Could not close the client socket");
  }

  if (close(sockfd) == -1) {
    perror("close(): Could not close the listening socket");
  }

  freeaddrinfo(res);                         /* free res */
  return 0;
}
