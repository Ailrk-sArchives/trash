#include <iostream>
#include <iterator>
#include <vector>

// Deduction guide for CONSTRUCTOR.
// Conclusion
// 1. constrain deduction of type parameters.
// 2. if some deduction guides are provided and non are satisfied,
//    type deduciton guide will failed.
// 3. use type_trait + type deduction we can constraint what type a type parameter
//    get deduced to without writing sfinae.

// now we defeind this template, but it's hard to deduce the type of Iter
// in the second constructor.
template <typename T> struct Container {
  Container(T t) {}

  template <typename Iter> Container(Iter beg, Iter end);
};

// we can write our own deduction guide.
// the guide works on the constructor...
template <typename Iter>
Container(Iter beg, Iter end)
    -> Container<typename std::iterator_traits<Iter>::value_type>;

// deduction guide for the first constructor.
// with this guide, no matter what number type are used, the container will
// deduce to Container<float> ...
#define EVERTHING_FLOAT
#ifdef EVERTHING_FLOAT
template <typename T> Container(T t) -> Container<float>;
#endif

Container c(7); // use implicit generate guide for consructor 1.

#ifdef EVERTHING_FLOAT
// this works because not type deduction invovles.
Container<std::vector<int>> k(std::vector<int>{});

// This doesn't work, because based on our deduciton guide it should be deduced
// to Container<float>, but there is no way to convert vector<int> to float.
// uncomment this will throw an error.

// Container k1(std::vector<int>{});
#endif

// this uses the deduction guide we provided above.
std::vector<char> v{};
auto d = Container(v.begin(), v.end());

// another nice thing is with the deduction guide, the second constructor only
// accept iterator now. Other types will fail by sfinae.
// this will fail
// Container e{1, 2};
