#include <cassert>
#include <cstdlib>

template <class T> class Vector {
 public:
  size_t capacity;
  size_t length;
  T* data;

  Vector();
  ~Vector();
  T* append(const size_t n = 1);
  void shrink(void);
};

template <class T> Vector<T>::Vector() : capacity(0), length(0), data(nullptr) {
}

template <class T> Vector<T>::~Vector() {
  if (data) {
    assert(capacity > 0);
    free(data);
  }
}

template <class T> T* Vector<T>::append(const size_t n) {
  if (length + n <= capacity)
    return &data[length];

  T* const new_data =
      static_cast<T*>(realloc(data, sizeof(T) * (capacity + n)));

  if (!new_data)
    return nullptr;

  data = new_data;
  capacity += n;

  return &data[length];
}

template <class T> void Vector<T>::shrink(void) {
  if (length == capacity)
    return;

  T* const new_data = static_cast<T* const>(realloc(data, sizeof(T) * length));

  if (!new_data)
    return;

  data     = new_data;
  capacity = length;
}
