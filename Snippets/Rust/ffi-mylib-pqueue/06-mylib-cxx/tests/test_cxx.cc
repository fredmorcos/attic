// Build using:
//
//   c++ -Wall -Wextra target/debug/libmylib.a -o tests/test_cxx
//
//   c++ -Wall -Wextra target/release/libmylib.a -o tests/test_cxx
//
// Running:
//
//   ./tests/test_auto
//
// With Valgrind (to show that really memory has been freed properly):
//
//   valgrind ./tests/test_auto

#include <array>
#include <cstdint>
#include <iostream>
#include <mylib/src/lib.rs.h>
#include <rust/cxx.h>

int main() {
  std::array<const uint8_t, 5> array{4, 2, 5, 1, 2};
  rust::Slice<const uint8_t> elements{array.data(), array.size()};
  rust::Box<Pq> pq = pq_create(elements);
  pq->push(6);
  for (int i = 0; i < 10; i++) {
    auto result = pq->pop();
    switch (result.status) {
    case PqPopStatus::Success:
      std::cout << "pop() = " << std::to_string(result.value) << std::endl;
      break;
    case PqPopStatus::Empty:
      std::cout << "pop() = Nothing" << std::endl;
      break;
    }
  }
}
