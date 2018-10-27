#include <stdio.h>
#include <err.h>
#include <sys/types.h>
#include <pwd.h>

int main (int argc, char **argv) {
  if (argc != 2)
    err(1, "getpwnam <username>");

  /* cppcheck-suppress getpwnamCalled */
  struct passwd *pw = getpwnam(argv[1]);

  if (!pw)
    errx(1, "getpwnam: no user");

  printf("user %s, uid %d, dir %s, shell %s\n",
         pw->pw_name,
         pw->pw_uid,
         pw->pw_dir,
         pw->pw_shell);

  return 0;
}
