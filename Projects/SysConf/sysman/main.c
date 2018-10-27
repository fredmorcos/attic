#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>
#include <pthread.h>

#include <sys/types.h>
#include <sys/stat.h>

void *threaded_func(void *);

void *threaded_func(void *arg) {
  syslog(LOG_NOTICE, "thread message\n");
  return;
}

int main (int argc, char **argv)
{
  pid_t pid, sid;               /* process ID, session ID */
  pthread_t tid;                /* thread ID */

  pid = fork();                 /* fork the parent process */

  if (pid < 0) {                /* forking error */
    exit(EXIT_FAILURE);         /* TODO: syslog the failure */
  } else if (pid > 0) {         /* forking worked */
    exit(EXIT_SUCCESS);         /* TODO: syslog the success */
  }

  umask(0);                     /* change file mode mask */

  /* open syslog */
  openlog(argv[0], LOG_NOWAIT | LOG_PID, LOG_DAEMON);
  syslog(LOG_NOTICE, "started\n");

  sid = setsid();               /* create a new session ID for child */

  if (sid < 0) {
    syslog(LOG_ERR, "Cannot create a new session ID\n");
    exit(EXIT_FAILURE);
  }

  if ((chdir("/")) < 0) {       /* change current working dir */
    syslog(LOG_ERR, "Cannot change dir to root\n");
    exit(EXIT_FAILURE);
  }

  /* close std file descriptors */
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);

  /* TODO: init daemon, read configs, etc... */

  syslog(LOG_NOTICE, "entering mainloop\n");

  while (1) {                   /* main daemon loop */
    /* TODO: real work here */
    pthread_create(&tid, NULL, threaded_func, (void *) NULL);
    sleep(3);
    syslog(LOG_NOTICE, "status message\n");
    break;
  }

  pthread_join(tid, NULL);
  syslog(LOG_NOTICE, "thread finished\n");

  syslog(LOG_NOTICE, "leaving\n");
  closelog();                   /* close syslog */
  exit(EXIT_SUCCESS);
}
