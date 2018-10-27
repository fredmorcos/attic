#pragma once

#include <netdb.h>

void net_close    (int *const fd);
int  net_listen   (const char *const port,
                   struct addrinfo **const ai,
                   struct addrinfo **const l);
int  net_accept   (const int sfd, struct sockaddr_storage *const addr);

void net_ai_free  (struct addrinfo **ai);
void net_ai_print (const char *const prefix, const char *const port,
                   const int fam, struct sockaddr *const addr);
