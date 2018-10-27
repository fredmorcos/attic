void dir_diff_action(const struct dir *const d,
                     const bool *const safe);
void dir_diff_print(const struct dir *const d);
int dir_diff(const struct array *const c, /* client */
             const struct array *const s, /* server */
             struct array *const r);      /* result */
