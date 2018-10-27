#include <cstdlib>

template<class T> class Array {
public:
  void  remove(const size_t i);
  void  revert(const size_t n);
  void  extend(const size_t n);
  T    *add(const size_t n, const bool zero = false);

private:
  explicit Array(const size_t _ilen, void (*_freeCB)(T *));
  ~Array();

  T *ptr;

  size_t len;
  size_t alen;
  size_t ilen;

  void (*freeCB)(T *);
};
