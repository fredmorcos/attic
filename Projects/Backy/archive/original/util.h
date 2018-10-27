typedef void *(*const thread_cb)(void *);

static const char nul = '\0';

void data_stats(const char *const prefix,
                const double seconds,
                const size_t size);
void element_stats(const char *const prefix,
                   const double seconds,
                   const size_t length);

double timediff(clock_t c);

__attribute__((malloc))
char *humansize(double size);

__attribute__((pure))
bool find_nchars(const char *const buf,
                 const size_t len,
                 const char ch,
                 const size_t num);

__attribute__((format (printf, 3, 4)))
int asprintf(size_t *const len,
             char **const str,
             const char *const fmt, ...);
