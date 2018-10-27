#include "Vector.hpp"
#include <iostream>

struct foo {
  int a;
  int b;
  int c;

  foo(int _a, int _b, int _c) : a(_a), b(_b), c(_c){};
};

int main(void) {
  Vector<foo> v = Vector<foo>();

  foo* const f = v.append();

  f->a = 1;
  f->b = 2;
  f->c = 3;

  v.length++;

  foo* const g = v.append(5);

  *g = foo(3, 4, 5);

  v.length++;

  v.shrink();

  for (size_t i = 0; i < v.length; i++) {
    std::cout << "a: " << v.data[i].a << ", b: " << v.data[i].b
              << ", c: " << v.data[i].c << std::endl;
  }
}
