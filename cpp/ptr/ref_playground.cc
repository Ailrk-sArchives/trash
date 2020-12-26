#include <iostream>

struct S {
  int i;
  S(int i) : i(i) {}
};

void f1_rvalref(S &&s) { std::cout << s.i << std::endl; }
void f2_constref(const S &s) { std::cout << s.i << std::endl; }
void f3_ref(S &s) { std::cout << s.i << std::endl; }
void f4_val(S s) { std::cout << s.i << std::endl; }

void pass_lval_to_rval_ref() {
  S s(1);
  // must move other wise you can't pass it.
  f1_rvalref(std::move(s));
}

void pass_temporary_to_const_ref() {
  // the lifetime of the temporary get extended to span the scope

  f2_constref(S(1));
}

void pass_value_to_const_ref() {
  // it wii just take a reference of the value.
  S s(1);
  f2_constref(s);
}

void pass_temporary_to_ref() {
  // reference can't extend the lifetime.
  // must pass lvalue ref
  S s(1);
  f3_ref(s);
}

void pass_ref_to_value() {
  // it will just take the reference.
  S s(1);
  S &sr = s;
  f4_val(sr);
}

// overload to make it works for both nonconst reference and rvalue reference
// take a value, modify it destructively, and then use the value internally.
void f1(int &v) {
  v += 1;
  std::cout << v << std::endl;
}
void f1(int &&v) { return f1(v); }

template <typename T, typename U> void f22(T &t, const U &u) {}
template <typename T, typename U> void f2(T &&t, const U &u) {
  f22(std::forward(t), u);
}

int main(void) {
  int a = 1;
  f1(a);
  f1(1);
  return 0;
}
