#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define LOGFILE "/daemon-test-log"

int main (int argc, char *argv[])
{
	pid_t pid, sid;
	char *home_dir, *log_path;
	struct passwd *user_info;
	FILE *log_file;
	int log_path_len;

	pid	= fork();
	if (pid < 0)
		return EXIT_FAILURE;

	if (pid > 0)
		return EXIT_SUCCESS;

	umask(0);

	sid = setsid();
	if (sid < 0)
		return EXIT_FAILURE;

	close(STDIN_FILENO);
	close(STDOUT_FILENO);
	close(STDERR_FILENO);

	user_info = getpwuid(getuid());
	home_dir = user_info->pw_dir;

	log_path_len = strlen(home_dir) + strlen(LOGFILE) + 1;
	log_path = malloc(log_path_len * sizeof(char));
	log_path[0] = '\0';
	log_path = strcat(log_path, home_dir);
	log_path = strcat(log_path, LOGFILE);

	log_file = fopen(log_path, "w+");

	if (!log_file)
	{
		free(log_path);
		return EXIT_FAILURE;
	}

	while (1)
	{
		fprintf(log_file, "I am the child process!\n");
	}

	fclose(log_file);

	return EXIT_SUCCESS;
}
