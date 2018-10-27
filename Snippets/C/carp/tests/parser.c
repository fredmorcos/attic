#include <carp/parser.h>
#include <buffer/buffer.h>
#include <test/test.h>
#include <talloc/talloc.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

int main (__attribute__((unused)) int argc,
          __attribute__((unused)) char **argv) {
  const char *const code = "\n\n# foobar\n\n#foo\n";
  struct buffer buf = { (void *) code, 1, strlen(code), strlen(code) };
  struct carp_parser parser = { &buf, buf.ptr, 1, 1, 0 };
  struct carp_parser_token *res = NULL;
  bool test_res = true;

  test_res &= test((res = carp_parser_parse(&parser)),
                   "Parser run succeeded",
                   "Parser error at %lu,%lu: %s\n",
                   parser.line, parser.col, parser.p);

  if (res) {
    tfree(res);
  }

  return !test_res;
}
