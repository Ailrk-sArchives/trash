#include <iostream>
#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

enum jvalue_tag { O, A, N, B, JNULL };

struct jnull_t {};

struct jobject_t;
struct jarray_t;

using jvalue_t = std::variant<std::string, int, bool, jobject_t *, jarray_t *>;

struct jobject_t {
  std::unordered_map<std::string, jvalue_t> v;
};

struct jarray_t {
  std::vector<jvalue_t> v;
};

int main(void) {
  jarray_t arr{.v = {1, 2, 3}};
  jvalue_t v{&arr};

  if (std::holds_alternative<jarray_t *>(v)) {
    auto a = std::get<jarray_t *>(v);
    auto b = std::get<int>(a->v[0]);
    std::cout << b << std::endl;
  }
  return 0;
}
