#include <iostream>
// https://www.researchgate.net/publication/221241323_A_comparison_of_C_concepts_and_Haskell_type_classes
// property based generic programming.

// Eight features of generic programming
// 1. Multi type concepts
// 2. multiple constraints
// 3. associated type access
// 4. retroactive modeling
// 5. type aliases
// 6. separate compilation
// 7. implicit instantiation
// 8. concise syntax

//////////////////////////////////////
// concepts help compiler to select appropriate overload.
// Intent of concept is to model semantic catagory. As type parameter T belongs
// to a set of types.

//////////////////////////////////////
// define concept.
// Concept will check the description and reject ilegal input accordingly.
// It's another system build on top of the duck typing based template
// paramter.
template <typename T> // require introduce a type level predicate.
concept Decremental = requires(T t) {
  --t;
};

// The concept Decremental `refines` RevIterator.
// Retoractive modeling
template <typename T>
concept RevIterator = Decremental<T> && requires(T t) {
  *t;
};

template <typename T, typename U>
concept Derived = std::is_base_of<U, T>::value;

template <typename T>
concept HasIterator = requires {
  typename T::iterator; // iterator shoule be a typename
};

template <typename T> // the entire require clause works the same as P(a)
concept Hashtable = requires(T a) {
  { std::hash<T>{}(a) } -> std::convertible_to<size_t>;
};

template <typename T>
concept Addable = requires(T a, T b) {
  {a + b};
};

template <typename T, typename U = T>
concept Swappable = requires(T &&t, U &&u) {
  swap(std::forward<T>(t), std::forward<U>(u));
  swap(std::forward<U>(u), std::forward<T>(t));
};

// Full Parametricity
template <typename T>
concept Tautology = true;

//////////////////////////////////////
// use constraints.

template <Decremental T> T bar(T t) { return --t; }
template <RevIterator T> T foo(T t) { return *t; }
template <HasIterator T> T gaw(T t) { return t; }
template <Hashtable T> T hashit(T t) { return std::hash<T>{}(t) + 1; }

// need top provide Base
template <typename Base, Derived<Base> T>  std::string issubtype(T t) {
  return "Yeah";
}

struct Dummy {
  using iterator = int;
  std::string to_string() { return "I'm dummy"; }
};

struct LilDummy : public Dummy {};

//////////////////////////////////////
int main(void) {

  Dummy d{};

  std::cout << "derement constraint: " << bar(10) << std::endl;
  std::cout << "hash constraint: " << hashit(10) << std::endl;

  // oh nice this compiles now.
  std::cout << gaw(Dummy()).to_string() << std::endl;
  return 0;
}
