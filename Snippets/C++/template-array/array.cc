#include "array.hh"
#include <cassert>

template<class T>
Array<T>::Array(const size_t _ilen, void (*_freeCB)(T *)):
  ptr(NULL), len(0), alen(0), ilen(_ilen), freeCB(_freeCB) {
}

template<class T>
Array<T>::~Array() {
  if (alen == 0) {
    assert(len == 0);
    assert(ptr == NULL);
  } else {
    T *const _ptr = ptr;

    if (freeCB)
      for (size_t i = 0; i < len; i++, _ptr++)
        free(_ptr);

    assert(ptr);
    free(ptr);
  }
}
