#include <iostream>

void check_static() {
  static int a = 23;
  a++;
  std::cout << a << std::endl;
}

int main(void)
{
  check_static();
  check_static();
  check_static();
  check_static();

  return 0;
}
