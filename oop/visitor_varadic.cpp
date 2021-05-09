#include <iostream>
#include <memory>

template <typename... Ts> class Visitable;

template <typename T> class Visitable<T> {
public:
  virtual void accept(T &) = 0;
};


