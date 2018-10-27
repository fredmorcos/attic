#pragma once

#include <stdbool.h>
#include <stdio.h>

FILE *open_file(const char *const file_name, const char *const mode);
char *strfmt(const char *format, ...);
char *read_file_str(const char *const file_name);
