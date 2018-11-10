#include <cstring>
#include <iostream>
#include <memory>
#include <string>

class Base {
public:
  explicit Base(const char *const msg):
    msg(msg),
    str(new char[6])
  {
    memcpy(str, "Hello", 6);
  }

  explicit Base(std::string msg):
    msg(std::move(msg)),
    str(nullptr)
  {
  }

  Base(const Base& src) = default;

  Base(Base&& src) noexcept:
    msg(std::move(src.msg)),
    str(src.str)
  {}

  ~Base() {
    delete[] str;
  }

  Base& operator=(const Base& src) = default;

  Base& operator=(Base&& src) noexcept {
    msg = std::move(src.msg);
    str = src.str;
    return *this;
  }

  friend std::ostream& operator<<(std::ostream& stream, const Base& base);

protected:
  std::string msg;

private:
  char *str;
};

std::ostream& operator<<(std::ostream& stream, const Base& base) {
  stream << base.msg;
  return stream;
}

class Child: public Base {
public:
  Child(const char *const msg, const char *const type):
    Base(msg),
    type(type)
  {}

  Child(const Child& src) = default;

  Child(Child&& src) = default;

  Child& operator=(const Child& src) {
    msg = src.msg;
    type = src.type;
    return *this;
  }

  Child& operator=(Child&& src) noexcept {
    msg = std::move(src.msg);
    type = std::move(src.type);
    return *this;
  }

  ~Child() = default;

  friend std::ostream& operator<<(std::ostream& stream, const Child& child);

protected:
  std::string type;
};

std::ostream& operator<<(std::ostream& stream, const Child& child) {
  stream << child.msg << " (" << child.type << ")";
  return stream;
}

Base new_base() {
  std::string msg("Hello world");
  return Base(msg);
}

class BaseUser {
public:
  explicit BaseUser(const Base& base) {
    std::cout << "BaseUser: " << base << std::endl;
  }
};

template<typename T, size_t len = 2> class Vec {
public:
  Vec():
    buf(new T[len])
  {
    for (size_t i = 0; i < len; i++) {
      buf[i] = i + 97;
    }
  }

  Vec(const Vec& src) = default;

  Vec(Vec&& src) noexcept = default;

  Vec& operator=(const Vec& src) {
    buf(src.buf);
    return *this;
  }

  Vec& operator=(Vec&& src) noexcept {
    buf = std::move(src.buf);
    return *this;
  }

  ~Vec() {
    delete[] buf;
  }

  void info() {
    std::cout << "Created a Vec of " << len << " elements:";
    for (size_t i = 0; i < len; i++) {
      std::cout << " \"" << buf[i] << "\"";
    }
    std::cout << std::endl;
  }

private:
  T *const buf;
};

// template<size_t len>
// class Vec<int, len> {
// };

int main () {
  Base base("Hello world from Base!");
  std::cout << base << std::endl;

  const Base& base_copy(base);
  std::cout << base_copy << std::endl;

  Child child = Child("Hello world from Child!", "Type Child");
  std::cout << child << std::endl;

  Base base2 = new_base();
  std::cout << base2 << std::endl;

  BaseUser bu(child);
  BaseUser bu2(base);

  Vec<std::string, 11> v1;
  v1.info();

  Vec<int, 42> v2;
  v2.info();
}
