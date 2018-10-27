#include "expense.h"
#include "extra.h"
#include "log.h"
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <talloc.h>
#include <assert.h>

static int exp_sort_tags_cmp (const void *p1, const void *p2);

bool exp_print_table (buf_t *ebuf) {
  const char
    *hdr_amount = "AMOUNT",
    *hdr_date   = "DATE",
    *hdr_person = "PERSON",
    *hdr_shop   = "SHOP",
    *hdr_tags   = "TAGS",
    *hdr_note   = "NOTE";

  int
    max_amount = strlen(hdr_amount),
    max_person = strlen(hdr_person),
    max_tags   = strlen(hdr_tags),
    max_shop   = strlen(hdr_shop);

  int tags_len = 0;
  int i = 0;

  char *tag_str = NULL;
  char *tag_strp = NULL;

  tok_t *tag_p = NULL;
  exp_t *end = NULL;
  exp_t *ep = NULL;

  struct stat st;

  assert(ebuf != NULL);
  assert(ebuf->ptr != NULL);

  end = (exp_t *) ebuf->ptr + ebuf->len;

  for (ep = ebuf->ptr; ep < end; ep++) {
    for (tags_len = ep->tags_buf->len - 1, i = 0; i < ep->tags_buf->len; i++) {
      tags_len += ((tok_t *) ep->tags_buf->ptr)[i].len;
    }

    max_amount = MAX(max_amount, ep->amount.len);
    max_person = MAX(max_person, ep->person.len);
    max_shop   = MAX(max_shop,   ep->shop.len);
    max_tags   = MAX(max_tags,   tags_len);
  }

  if (fstat(STDOUT_FILENO, &st) != 0) {
    return false;
  }

  if (S_ISCHR(st.st_mode)) {
    printf("  %*s    %*s    %*s    %*s    %-*s    %s\n",
           max_amount, hdr_amount,
           10,         hdr_date,
           max_person, hdr_person,
           max_shop,   hdr_shop,
           max_tags,   hdr_tags,
           hdr_note);
  }

  if (!(tag_str = (char *) malloc(sizeof(char) * max_tags))) {
    return false;
  }

  tag_str = memset(tag_str, 0, sizeof(char) * max_tags);

  for (ep = ebuf->ptr; ep < end; ep++) {
    for (tag_strp = tag_str, i = 0; i < ep->tags_buf->len; i++) {
      tag_p = &((tok_t *) ep->tags_buf->ptr)[i];
      strncpy(tag_strp, tag_p->ptr, (size_t) tag_p->len);
      tag_strp += (sizeof(char) * tag_p->len);

      if (i != ep->tags_buf->len - 1) {
        *tag_strp = ',';
        tag_strp++;
      }
    }

    for (; tag_strp < tag_str + (sizeof(char) * max_tags); tag_strp++) {
      *tag_strp = ' ';
    }

    printf("  %*.*s    %.*s-%.*s-%.*s    %*.*s    %*.*s    %.*s    %.*s\n",
           max_amount, ep->amount.len, ep->amount.ptr,
                       ep->year.len,   ep->year.ptr,
                       ep->month.len,  ep->month.ptr,
                       ep->day.len,    ep->day.ptr,
           max_person, ep->person.len, ep->person.ptr,
           max_shop,   ep->shop.len,   ep->shop.ptr,
           max_tags,                   tag_str,
                       ep->note.len,   ep->note.ptr);
  }

  if (fflush(stdout) == EOF) {
    log_syserr(errno, "Cannot flush stdout");
  }

  free(tag_str);
  return true;
}

static int exp_sort_tags_cmp (const void *p1, const void *p2) {
  const tok_t *t1 = p1;
  const tok_t *t2 = p2;

  assert(p1 != NULL);
  assert(p2 != NULL);

  return strncasecmp(t1->ptr, t2->ptr, MIN(t1->len, t2->len));
}

void exp_sort_tags (const buf_t *tags) {
  assert(tags != NULL);
  qsort(tags->ptr, (size_t) tags->len, tags->esize, exp_sort_tags_cmp);
}

int exp_buf_destr (buf_t *buf) {
  exp_t *e;

  assert(buf != NULL);
  assert(buf->ptr != NULL);

  e = buf->ptr;

  for (int i = 0; i < buf->len; i++, e++) {
    if (e->tags_buf) {
      TALLOC_FREE(e->tags_buf);
    }
  }

  return 0;
}
