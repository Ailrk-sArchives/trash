#include <iostream>
#include <sstream>
#include <string>

class Str : public std::string {

public:
  Str(const Str &a) : std::string(a) { std::cout << "copyed" << std::endl; }

  Str(const char *str) : std::string(str) {}
};

Str operator+(Str &&a, Str &&b) {
  std::stringstream os;
  std::cout << "in move addition" << std::endl;
  os << a << b;
  return Str(os.str().c_str());
}

int main(void) {

  Str s1("abc");
  Str s2("cde");
  Str s3(s1);

  auto c = std::move(s1) + std::move(s2);
  std::cout << "result: " << c << std::endl;

  return 0;
}
