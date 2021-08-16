#include <algorithm>
#include <concepts>
#include <span>
#include <vector>

template <typename T>
concept IsStack = requires(T t) {
  typename T::type;
  typename T::stack_type;
  requires requires(typename T::type t1) {
    { t.push(t1) } -> std::same_as<void>;
  };
  { t.pop() } -> std::same_as<void>;
  { t.peek() } -> std::same_as<std::span<typename T::type>>;
  { t.top() } -> std::same_as<typename T::type>;
};

struct IntStack {
  using type = int;
  using stack_type = std::vector<type>;

  stack_type stack;
  void push(type n) { stack.push_back(n); }
  void pop() { stack.pop_back(); }
  std::span<type> peek() { return std::span<type>{stack}; }
  type top() { return stack.back(); }
};

template <IsStack S> void fill_8(S &s, int n) {
  for (int i = 0; i < 8; ++i) {
    s.push(8);
  }
}

template <IsStack S> void pop_5(S &s) {
  for (int i = 0; i < 5; ++i) {
    s.pop(8);
  }
}

template <int Num, IsStack Stack> struct Const {
  int idx = Num;
  typename Stack::type value;
  std::string to_string() const;
  void *codegen() const;
  typename Stack::type evaluate(Stack &s) const;
};


template <IsStack Stack> struct Const<0, Stack> {
  int idx = 0;
  typename Stack::type value;
  std::string to_string() const;
  void *codegen() const;
  typename Stack::type evaluate(Stack &s) const;
};

/* template <IsStack S> */
/* void * Const<0, S>::codegen() { */

/* } */

int main() {
  IntStack s;
  fill_8(s, 8);
  //   pop_5(s);
}
