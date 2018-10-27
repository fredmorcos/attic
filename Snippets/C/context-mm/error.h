#ifndef ERROR_H
#define ERROR_H

enum error_type {
  Warning,
  Error
};

struct error_t {
  int    user_errno;
  char * user_message;

  int    sys_errno;

  char * code_filename;
  char * code_function;
  int    code_line;
};

#endif  /* ERROR_H */
