#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VERSION 1

void usage();
void consume_argv(int, char **);

int main(int argc, char **argv)
{
		if (argc < 2 || strcmp("help", argv[1]) == 0) {
				usage();
				return EXIT_FAILURE;
		} else {
				consume_argv(argc, argv);
		}

		return EXIT_SUCCESS;
}

void usage() {
    printf("Personal Expense Tracker version %d\n", VERSION);
    printf("Fred Morcos  2012-2013\n");
    printf("https://github.com/fredmorcos/pet.git\n");
    printf("\n");
    printf("usage: pet [COMMAND] [ARGS]\n");
    printf("\n");
    printf("COMMANDS\n");
    printf("  help       show this help\n");
    printf("  show FILE  show expenses from FILE\n");
}

void consume_argv(int argc, char **argv)
{
    if (strcmp(argv[1], "show") == 0)
        return;
}
