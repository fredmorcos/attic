struct dir_get_params {
  pthread_t thread;
  bool cflag;                           /* thread cancellation flag */

  const char *const path;
  struct array *res;
  int ret;
};

int dir_get(const bool *const cflag,
            const char *const path,
            struct array *const res);
void *dir_get_thread(struct dir_get_params *const params);
