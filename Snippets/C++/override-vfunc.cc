#include <iostream>
#include <memory>

using std::cout;
using std::endl;

class Base {
public:
  virtual ~Base() {
    cout << "Delete Base" << endl;
  }

  auto doSomething(void) -> void {
    cout << "Base" << endl;
  }
};

class Derived: public Base {
public:
  virtual ~Derived() {
    cout << "Delete Derived" << endl;
  }

  virtual auto doSomething(void) -> void {
    cout << "Derived" << endl;
  }
};

int main() {
  std::unique_ptr<Base> b = std::make_unique<Derived>();
  b->doSomething();

  return 0;
}
