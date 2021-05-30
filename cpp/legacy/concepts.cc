template <typename T> concept Decremental = requires(T t) { --t; };
template <typename T> concept RevIterator = Decremental<T> &&requires(T t) {
  *t;
};

template <Decremental T> void f(T);
template <RevIterator T> void f(T);
