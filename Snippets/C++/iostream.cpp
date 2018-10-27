#include <iostream>

int main (int argc, char **argv) {
  int foo;
  std::cout << "Enter a number: ";
  std::cin  >> foo;
  std::cout << "The number is `" << foo << "`\n";
  return 0;
}
