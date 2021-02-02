// so called associative data type, just this
#include <iostream>
#include <map>
#include <unordered_map>

template <typename K> class GMapKey {
public:
  template <typename V> using GMap = void;
  template <typename V> static V lookup(K k, GMapKey<V>);
  template <typename V> static GMap<V> insert(K k, V v, GMapKey<V>);
};

template <> class GMapKey<int> {
public:
  template <typename V> using GMap = std::map<int, V>;
  template <typename V> static V lookup(auto k, GMap<V> m) { return m[k]; }
  template <typename V> static GMap<V> insert(auto k, V v, GMap<V> m) {
    m.insert(std::pair<int, V>(k, v));
    return m;
  }
};

template <> class GMapKey<char> {
public:
  template <typename V> using GMap = std::map<char, V>;
  template <typename V> static V lookup(auto k, GMap<V> m) { return m[k]; }
  template <typename V> static GMap<V> insert(auto k, V v, GMap<V> m) {
    m.insert(std::pair<char, V>(k, v));

    return m;
  }
};

int main(void) {
  std::map<int, char> m{};
  m = GMapKey<int>::insert(3, 'a', m);
  m = GMapKey<int>::insert(4, 'b', m);
  m = GMapKey<int>::insert(5, 'c', m);
  std::cout << GMapKey<int>::lookup(3, m) << std::endl;

  return 0;
}
