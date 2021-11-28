#include <iostream>
#include <memory>
#include <exception>
#include <stdexcept>

using std::cout;
using std::endl;

struct Something {
  explicit Something(int i_): i(i_) {
    cout << "Something(" << i << ")" << endl;
  }

  ~Something() {
    cout << "~Something(" << i << ")" << endl;
  }

private:
  int i;
};

auto throw_exception() -> void {
  throw std::out_of_range("something is out of range");
}

auto call() -> void {
  auto s0 = std::make_unique<Something>(0);
  try {
    auto s1 = std::make_unique<Something>(1);
    throw_exception();
  } catch (std::exception &e) {
    cout << "Error: " << e.what() << endl;
  }
}

int main()
{
  call();
  return 0;
}
