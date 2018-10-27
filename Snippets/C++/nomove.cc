#include <iostream>
#include <string>
#include <memory>

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::shared_ptr;

class Owned {
private:
  string *const m_name;

public:
  explicit Owned(string& name): m_name(new string(name)) {
    cout << "Create Owned object with name " << *m_name << endl;
  }

  const string& getName() const {
    return *m_name;
  }

  ~Owned() {
    delete m_name;
  }
};

class Owner {
private:
  // Owned m_owned;
  shared_ptr<Owned> m_owned;

public:
  // explicit Owner(Owned owned): m_owned(owned) {
  //   cout << "Create Owner with Owned object named " << m_owned.getName() << endl;
  // }

  explicit Owner(shared_ptr<Owned> owned): m_owned(owned) {
    cout << "Create Owner with Owned object named " << m_owned.get()->getName() << endl;
  }
};

int main (void) {
  string owned_name = "Megatrans";
  // Owned owned(owned_name);
  shared_ptr<Owned> owned = std::make_shared<Owned>(owned_name);
  Owner owner(owned);
  return 0;
}
