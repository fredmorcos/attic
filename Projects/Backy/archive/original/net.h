int socket_sendbuf(const int fd, const char *const buf, const size_t len);
int socket_recvbuf(const int fd, const size_t chunk, struct array *const buf);
void socket_close(const int fd, const char *const desc);
int socket_connect(const char *const host, const char *const port);
int socket_accept(const int sfd);
int socket_listen(const char *const port);
