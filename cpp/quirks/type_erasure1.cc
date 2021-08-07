#include <iostream>

// Type erasure to link dynamic dispatching and monomophization.
// - it's Existential type.
// - existential type is opqaue.
// - Callback is not templated.
// - but's constructor is templated.
// - once the constructor is called, type of the paramter is deduced.
// - the type then just erase in the pov from Callback.
// - We use callback with operator(), without knowing what T is.

struct Add1 {
  int call(int x) const { return x + 1; }
};

struct Sub1 {
  int call(int x) const { return x - 1; }
};

struct ICallback {
  virtual int call(int) const = 0;
};

// well this is still not ideal. The type parameter is not erased at the type level,
template <typename T> struct WrappingCallback : ICallback {
  const T *cb_;

  explicit WrappingCallback(T &&cb) : cb_(new T{std::forward<T>(cb)}) {}
  int call(int x) const override { return cb_->call(x); }
};

int run_twice(const ICallback &callback) {
  return callback.call(1) + callback.call(2);
}

int main(void) {
  auto a = run_twice(WrappingCallback(Add1{}));
  auto b = run_twice(WrappingCallback(Sub1{}));

  return 0;
}
