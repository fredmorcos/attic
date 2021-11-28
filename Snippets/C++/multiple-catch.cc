#include <iostream>
#include <memory>
#include <exception>
#include <stdexcept>

using std::cout;
using std::endl;

struct Foo {
  explicit Foo(int x_): x(x_) {}
  explicit Foo(): x(0) {}

  auto set_x(int x_) -> void {
    x = x_;
  }

  auto get_x() -> int {
    return x;
  }

  ~Foo() {
    cout << "~Foo(" << x << ")" << endl;
  }

private:
  int x;
};

auto f() -> void {
  auto f = std::make_unique<Foo>();
  throw std::out_of_range("something out of range");
}

auto main() -> int
{
  try {
    auto x = std::make_unique<Foo>(1);
    f();
  } catch (std::out_of_range &e) {
    cout << "out of range: " << e.what() << endl;
  } catch (std::exception &e) {
    cout << "exception: " << e.what() << endl;
  }

  return 0;
}
