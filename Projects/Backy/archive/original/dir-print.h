struct dir_print_params {
  pthread_t thread;
  bool cflag;                           /* thread cancellation flag */

  bool is_action_list;
  const char *const path;
  size_t *const len;
};

char *dir_sprint(const bool is_action_list,
                 struct array *const dirs,
                 size_t *const len_);
char *dir_print(const bool is_action_list,
                const bool *const cflag,
                const char *const path,
                size_t *const len);
void *dir_print_thread(struct dir_print_params *const params);
