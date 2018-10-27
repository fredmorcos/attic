#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int main(void)
{
    FILE* fp = fopen("test.txt", "r");
    if(fp == NULL) {
       perror("fopen"); return 1;
    }

    struct stat stats;
    if(fstat(fileno(fp), &stats) == -1) { // POSIX only
        perror("fstat"); return 1;
    }

    printf("BUFSIZ is %d, but optimal block size is %ld\n", BUFSIZ, stats.st_blksize);
    if(setvbuf(fp, NULL, _IOFBF, stats.st_blksize) != 0) {
       perror("setvbuf failed"); // POSIX version sets errno
       return 1;
    }

    int ch;
    while((ch=fgetc(fp)) != EOF); // read entire file: use truss/strace to
                                  // observe the read(2) syscalls used
}
