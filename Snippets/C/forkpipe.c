/*
 * gcc -Wall -Wextra -pedantic -std=c11 forkpipe.c -o forkpipe
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <sys/wait.h>

int main(int argc, __attribute__((unused)) char *argv[argc + 1]) {
  int cpid;
  int stat;

  int p[2];

  char buf[200];
  int n;

  const char *echo_args[] = {"/usr/bin/echo", "hello world!", NULL};

  pipe(p);

  switch((cpid = fork())) {
  case -1:                                   /* error */
    perror("fork()");
    exit(EXIT_FAILURE);
  case 0:                                    /* child */
    close(p[0]);
    dup2(p[1], STDOUT_FILENO);
    execv(echo_args[0], (char *const *) echo_args);
    exit(EXIT_SUCCESS);
  default:                                   /* parent */
    close(p[1]);
    printf("child pid = %d\n", cpid);
    n = read(p[0], buf, 200);
    close(p[0]);
    buf[n] = '\0';
    waitpid(cpid, &stat, 0);
    printf("child finished\n");
    printf("output = %s\n", buf);
    exit(EXIT_SUCCESS);
  }
}
