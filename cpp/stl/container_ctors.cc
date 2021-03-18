#include <iostream>
#include <vector>
void print_vec(const std::vector<int> &);

void mk_string() {
  // normal
  std::string s1;
  std::string s2{"str"};
  std::string s3{s2};
  std::string s4 = s3;

  // ptr
  const char *se = "abc";
  std::string s5{se, 10};

  // fill
  std::string s6{10, 'a'};

  // iter
  std::vector<char> v{'a', 'z'};
  std::string s7{v.begin(), v.end()};

  std::cout << s1 << "\n"
            << s2 << "\n"
            << s3 << "\n"
            << s4 << "\n"
            << s5 << "\n"
            << s6 << "\n"
            << s7 << "\n"
            << std::endl;
}

void mk_vec() {
  std::vector<int> v1;

  // init list
  std::vector<int> v2{1, 2, 3, 4};

  // fill
  // note it needs to use bracket.
  std::vector<int> v3(10, 99);

  // range
  std::vector<int> v4{v3.begin(), v3.begin() + 3};

  print_vec(v1);
  print_vec(v2);
  print_vec(v3);
  print_vec(v4);
}

void print_vec(const std::vector<int> &v) {
  for (auto i : v) {
    std::cout << i << ", ";
  }
  std::cout << "\n" << std::endl;
}

int main() {
  mk_string();
  mk_vec();
}
