#include <assert.h>
#include <dirent.h>
#include <bsd/bsd.h>

int dirload_str(DIR *const dir,
                const char *const dirname,
                const size_t dirname_len,
                struct dirent *const ent,
                struct vec *const str) {
  struct dirent *res = NULL;

  assert(dir);
  assert(dirname);
  assert(dirname_len > 0);
  assert(ent);
  assert(str->esize == sizeof(char));

  while (readdir_r(dir, ent, &res) == 0) {
    if (!res)
      return 0;

    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    if (ent->d_type == DT_DIR) {
      DIR *sdir = NULL;
      char *sdirname = NULL;
      size_t sdirname_len = 0;

      int dir_fd = -1;
      int sdir_fd = -1;

      int ret = 0;
      int new_ret = 0;

      char *strp = NULL;

      static const char nil = '\0';

      ret = snprintf(NULL, 0, "+%c%s", nil, ent->d_name);

      if (ret < 0) {
        warnx("E: Cannot prepare dir info: %s/%s", dirname, ent->d_name);
        return -1;
      }

      if (!(strp = vec_addn(str, (size_t) ret + 1))) {
        warn("E: Cannot allocate dir info: %s/%s", dirname, ent->d_name);
        return -1;
      }

      new_ret = snprintf(strp, (size_t) ret + 1, "+%c%s", nil, ent->d_name);

      if (new_ret < 0 || (size_t) new_ret >= (size_t) ret + 1) {
        warnx("E: Cannot write dir info: %s/%s", dirname, ent->d_name);
        vec_revn(str, (size_t) ret + 1);
        return -1;
      }

      if ((dir_fd = dirfd(dir)) == -1) {
        warn("E: Cannot get directory descriptor: %s", dirname);
        return -1;
      }

      if (!(sdir = opendir(ent->d_name))) {
        warn("E: Cannot open directory: %s/%s", dirname, ent->d_name);
        return -1;
      }

      if ((sdir_fd = dirfd(sdir)) == -1) {
        warn("E: Cannot get directory descriptor: %s/%s", dirname, ent->d_name);

        if (closedir(sdir) != 0)
          warn("W: Cannot close directory: %s/%s", dirname, ent->d_name);

        return -1;
      }

      if (fchdir(sdir_fd) == -1) {
        warn("E: Cannot change into directory: %s/%s", dirname, ent->d_name);

        if (closedir(sdir) != 0)
          warn("W: Cannot close directory: %s/%s", dirname, ent->d_name);

        return -1;
      }

      /* here +1 is for the path delimiter "/" */
      sdirname_len = dirname_len + strlen(ent->d_name) + 1;

      if (!(sdirname = reallocarray(NULL, sdirname_len + 1, sizeof(char)))) {
        warn("E: Cannot allocate space for path: %s/%s", dirname, ent->d_name);

        if (closedir(sdir) != 0)
          warn("W: Cannot close directory: %s/%s", dirname, ent->d_name);

        return -1;
      }

      ret = snprintf(sdirname, sdirname_len + 1, "%s/%s", dirname, ent->d_name);

      if (ret < 0 || (size_t) ret >= sdirname_len + 1) {
        warnx("E: Cannot build path: %s/%s", dirname, ent->d_name);
        free(sdirname);

        if (closedir(sdir) != 0)
          warn("W: Cannot close directory: %s/%s", dirname, ent->d_name);

        return -1;
      }

      if ((ret = dirload_str(sdir, sdirname, sdirname_len, ent, str)) != 0)
        return ret;

      free(sdirname);

      if (closedir(sdir) != 0)
        warn("W: Cannot close directory: %s/%s", dirname, ent->d_name);

      if (fchdir(dir_fd) == -1) {
        warn("E: Cannot change back to directory: %s", dirname);
        return -1;
      }

      ret = snprintf(NULL, 0, "-");

      if (ret < 0) {
        warnx("E: Cannot prepare dir info term: %s/%s", dirname, ent->d_name);
        return -1;
      }

      if (!(strp = vec_addn(str, (size_t) ret + 1))) {
        warn("E: Cannot allocate dir info term: %s/%s", dirname, ent->d_name);
        return -1;
      }

      new_ret = snprintf(strp, (size_t) ret + 1, "-");

      if (new_ret < 0 || (size_t) new_ret >= (size_t) ret + 1) {
        warnx("E: Cannot write dir info term: %s/%s", dirname, ent->d_name);
        vec_revn(str, (size_t) ret + 1);
        return -1;
      }
    } else if (ent->d_type == DT_LNK) {
      /* printf("L %s\n", ent->d_name); */
    } else if (ent->d_type == DT_REG) {
      /* printf("F %s\n", ent->d_name); */
    } else if (ent->d_type == DT_UNKNOWN) {
      printf("? %s\n", ent->d_name);
    } else {
      warnx("W: Skipping unsupported filetype (%d) for %s\n",
            ent->d_type, ent->d_name);
    }
  }

  return -1;
}
