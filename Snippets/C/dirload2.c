#include <assert.h>
#include <errno.h>
#include <dirent.h>
#include <bsd/bsd.h>

int dirload_dirend_str(const struct dirent *const ent,
                       struct vec *const str) {
  char *str_p = NULL;

  if (!(str_p = vec_addn(str, 2))) {
    warn_realpath("E: Cannot allocate string for dir info", ent->d_name);
    return -3;
  }

  *str_p++ = '-';
  *str_p++ = '\0';

  return 0;
}

int dirload_dir_str(const struct dirent *const ent,
                    const size_t ent_name_len,
                    struct vec *const str) {
  const size_t len = 2 + ent_name_len + 1;
  char *str_p = NULL;

  if (!(str_p = vec_addn(str, len))) {
    warn_realpath("E: Cannot allocate string for dir info", ent->d_name);
    return -3;
  }

  *str_p++ = '+';
  *str_p++ = '\0';

  if (strlcpy(str_p, ent->d_name, ent_name_len + 1) >= ent_name_len + 1) {
    warnx("E: Not enough space for dir name of length %zu", ent_name_len + 1);
    vec_revn(str, len);
    return -4;
  }

  return 0;
}

int dirload_str(const char *const dirname,
                struct dirent *const ent,
                struct vec *const str) {
  int ret = -1;                         /* unknown error (default) */
  int sub_ret = 0;                      /* readdir_r, dirload_str */
  int tmp_errno = 0;

  DIR *dir = NULL;
  int dir_fd = 0;

  struct dirent *res = NULL;

  assert(dirname);
  assert(ent);
  assert(str);
  assert(str->esize == sizeof(char));

  if (!(dir = opendir(dirname))) {
    warn_realpath("E: Cannot open directory", dirname);
    return -2;                        /* dir/file operation failure */
  }

  if ((dir_fd = dirfd(dir)) == -1) {
    warn_realpath("E: Cannot get directory descriptor", dirname);
    ret = -2;
    goto finish;
  }

  if (fchdir(dir_fd) == -1) {
    warn_realpath("E: Cannot change into directory", dirname);
    ret = -2;
    goto finish;
  }

  while ((sub_ret = readdir_r(dir, ent, &res)) == 0) {
    if (!res) {
      ret = 0;                          /* success */
      goto finish;
    }

    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    const size_t d_name_len = strlen(ent->d_name);

    if (ent->d_type == DT_DIR) {
      if ((ret = dirload_dir_str(ent, d_name_len, str)) < 0)
        goto finish;

      sub_ret = dirload_str(ent->d_name, ent, str);
      tmp_errno = errno;

      if (fchdir(dir_fd) == -1) {
        warn_realpath("E: Cannot change out of directory", ent->d_name);
        ret = -2;
        goto finish;
      }

      if (sub_ret < 0) {
        errno = tmp_errno;
        ret = sub_ret;
        goto finish;
      }

      if ((ret = dirload_dirend_str(ent, str)) < 0)
        goto finish;
    } else if (ent->d_type == DT_LNK) {
      /* printf("L %s\n", ent->d_name); */
    } else if (ent->d_type == DT_REG) {
      /* printf("F %s\n", ent->d_name); */
    } else if (ent->d_type == DT_UNKNOWN) {
      /* printf("? %s\n", ent->d_name); */
    } else {
      char *fname = NULL;

      warnx("W: Skipping unsupported filetype (%d) for %s",
            ent->d_type, realpath_s(ent->d_name, &fname) ? fname : ent->d_name);

      if (fname)
        free(fname);
    }
  }

  /* here readdir_r() returned a positive error value */
  warnc_realpath(sub_ret, "E: Cannot read directory contents", dirname);

 finish:
  if (closedir(dir) != 0)
    warn_realpath("E: Cannot close directory", dirname);
  return ret;
}
