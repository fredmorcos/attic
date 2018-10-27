#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

struct person {
  size_t age;
  size_t foo;
};

int main(void) {
  int pfds[2];
  int rpfds[2];
  pid_t pid;

  if (pipe(pfds) == -1) {
    perror("pipe");
    return 1;
  }

  if (pipe(rpfds) == -1) {
    perror("reverse pipe");
    return 1;
  }

  if ((pid = fork()) == -1) {
    perror("fork");
    return 1;
  } else if (pid == 0) {
    printf("I am the child\n");
    close(pfds[1]);
    char buf[5];
    read(pfds[0], buf, 5);
    printf("Child: I finished reading from the pipe: %s\n", buf);
    struct person person;
    read(pfds[0], &person, sizeof(struct person));
    printf("Child: I finished reading a person from the pipe!\n");
    printf("  Age = %zu, Foo = %zu\n", person.age, person.foo);
    close(pfds[0]);

    close(rpfds[0]);
    person.age = 12;
    person.foo = 50;
    printf("Child: Sending back another person over the reverse pipe!\n");
    printf("  Age = %zu, Foo = %zu\n", person.age, person.foo);
    write(rpfds[1], &person, sizeof(struct person));
    printf("Child: I sent a person over the pipe.\n");
    close(rpfds[1]);
  } else {
    printf("I am the parent, child pid = %d\n", pid);
    close(pfds[0]);
    write(pfds[1], "Test", 5);
    printf("Parent: I finished writing to the pipe, going to sleep.\n");
    sleep(3);
    struct person person = { 25, 30 };
    write(pfds[1], &person, sizeof(struct person));
    printf("Parent: I sent a person over the pipe.\n");
    close(pfds[1]);

    close(rpfds[1]);
    read(rpfds[0], &person, sizeof(struct person));
    printf("Parent: I finished reading a person from the pipe!\n");
    printf("  Age = %zu, Foo = %zu\n", person.age, person.foo);
    close(rpfds[0]);
  }

  return 0;
}
