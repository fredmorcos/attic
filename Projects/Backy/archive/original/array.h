struct array {
  void *ptr;                            /* ptr to C array */
  size_t len;                           /* used len */
  size_t alen;                          /* alloc'ed len */
  size_t esize;                         /* element size */
  void (*free_cb)(void *);              /* free callback */
};

typedef void (*const array_cb)(void *);
typedef void (*const array_cb1)(void *, void *);

void array_init(struct array *const a, const size_t esize, array_cb cb);
void array_revertn(struct array *const a, const size_t n);
bool array_addn(struct array *const a,
                const size_t n,
                const bool zero,
                void **const res);
void array_traverse(const struct array *const a,
                    void (*traverse_cb)(void *));
void array_traverse1(const struct array *const a,
                     void (*traverse_cb1)(void *, void *),
                     void *const arg1);
void array_free(struct array *const a);
