#include <algorithm>
#include <iostream>
#include <limits>
#include <vector>

auto merge_sort(std::vector<int> v1, std::vector<int> v2) -> std::vector<int> {
  auto v1_iter = v1.begin();
  auto v2_iter = v2.begin();
  std::vector<int> result{};

  while (1) {
    if (v1_iter == v1.end() && v2_iter == v2.end()) {
      break;
    }

    if (v1_iter == v1.end()) {
      result.push_back(*v2_iter);
      v2_iter++;
      continue;
    }

    if (v2_iter == v2.end()) {
      result.push_back(*v1_iter);
      v1_iter++;
      continue;
    }

    if (*v1_iter < *v2_iter) {
      result.push_back(*v1_iter);
      ++v1_iter;
    } else {
      result.push_back(*v2_iter);
      ++v2_iter;
    }
  }

  return result;
}

int main() {
  std::vector<int> v1{1, 6, 124, 2345, 2354, 3453, 5645};
  std::vector<int> v2{3, 12, 124, 312, 345, 3453, 12343};

  auto result = merge_sort(std::move(v1), std::move(v2));
  std::cout << "Result = { ";

  for (auto element : result) {
    std::cout << element << " ";
  }

  std::cout << "}" << std::endl;

  return 0;
}
