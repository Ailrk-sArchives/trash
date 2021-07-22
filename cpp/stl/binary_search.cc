#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

int main(void) {

  {
    std::cout << "[upper bound test 1]" << std::endl;
    std::cout << "data: 1, 3, 7, 7, 9, 10" << std::endl;
    std::vector<int> v{1, 3, 7, 7, 9, 10};
    // found
    auto it = std::upper_bound(v.begin(), v.end(), 8);
    std::cout << "looking for 8" << std::endl;
    if (it == v.end()) {
      std::cout << " |- END" << std::endl;
    } else {
      std::cout << " |- Not END, value is: " << *it << std::endl;
      std::cout << "  |- value of it - 1 is:  " << *(it - 1) << std::endl;
    }

    it = std::upper_bound(v.begin(), v.end(), 12);
    std::cout << "looking for 12" << std::endl;
    if (it == v.end()) {
      std::cout << " |- END" << std::endl;
    } else {
      std::cout << " |- Not END, value is: " << *it << std::endl;
    }
  }
  std::cout << std::endl;

  {
    std::cout << "[lower bound test 2]" << std::endl;
    std::cout << "data: 1, 3, 7, 7, 9, 10" << std::endl;
    std::vector<int> v{1, 3, 7, 7, 9, 10};
    // found
    auto it = std::lower_bound(v.begin(), v.end(), 8);
    std::cout << "looking for 8" << std::endl;
    if (it == v.end()) {
      std::cout << " |- END" << std::endl;
    } else {
      std::cout << " |- Not END, value is: " << *it << std::endl;
      std::cout << "  |- value of it - 1 is:  " << *(it - 1) << std::endl;
    }

    it = std::lower_bound(v.begin(), v.end(), 12);
    std::cout << "looking for 12" << std::endl;
    if (it == v.end()) {
      std::cout << " |- END" << std::endl;
    } else {
      std::cout << " |- Not END, value is: " << *it << std::endl;
    }
  }

  return 0;
}
