#include <cstring>
#include <iostream>
#include <type_traits>

// moving elements from one location to another and destory the old instances is
// callled relocate.

namespace stdx {
template <typename It>
using iterator_value_type_t = typename std::iterator_traits<It>::value_type;

template <typename It>
using iterator_category_t =
    typename std::iterator_traits<It>::iterator_category;

template <typename It>
struct is_random_access_iterator
    : std::is_base_of<std::random_access_iterator_tag,
                      stdx::iterator_category_t<It>> {};

// a hierachy of subtyping.
template <size_t K> struct priority_tag : priority_tag<K - 1> {};
template <> struct priority_tag<0> {};

// if it's a pointer the container is contiguous.
template <typename It> struct is_contiguous_iterator_v : std::is_pointer<It> {};

// relocatble means both trivially constructable and destructable.
/* template <typename T> */
/* struct is_trivially_relocatable */
/*     : std::bool_constant<std::is_trivially_constructible_v<T> && */
/*                          std::is_trivially_destructible_v<T>> {}; */

template <typename T>
auto helper(priority_tag<1>)
    -> std::bool_constant<T::is_trivially_relocatable::value>;

template <typename T>
auto helper(priority_tag<0>)
    -> std::bool_constant<std::is_trivially_move_constructible_v<T> &&
                          std::is_trivially_destructible_v<T>>;

// if specialization for priority_tag<1> failes, it will try specialization
// for priority_tag<0>.
// If we have other specialization for specific T we will use that instead.
// otherwise we falls back to the default.
template <typename T>
struct is_trivially_relocatable : decltype(helper<T>(priority_tag<1>{})) {};
}; // namespace stdx

template <typename It, typename FwdIt>
FwdIt uninitialized_relocate(It first, It last, FwdIt dest) {
  using T = typename std::iterator_traits<FwdIt>::value_type;
  std::allocator<T> alloc;
  return __unitalized_relocate_a(first, last, dest, alloc);
}

template <typename It, typename FwdIt, typename Alloc>
FwdIt __unitalized_relocate_a(It first, It last, FwdIt dest, Alloc &a) {
  using T = typename std::iterator_traits<FwdIt>::value_type;
  static_assert(std::is_same_v<T, typename std::allocator<Alloc>::value_type>);

  constexpr bool is_simple_memcpy =
      std::is_same_v<T, stdx::iterator_value_type_t<It>> &&
      /* stdx::is_contiguous_iterator_v<It> && */
      /* stdx::is_contiguous_iterator_v<FwdIt> && */
      std::allocator<Alloc>::template has_trivial_construct_and_destory_v<T> &&
      stdx::is_trivially_relocatable<T>::value;

  if constexpr (is_simple_memcpy) {
    auto count = last - first;
    memcpy(std::addressof(*dest), std::addressof(*first), count * sizeof(T));
    return (dest + count);
  } else {
    while (first != last) {
      std::allocator_traits<Alloc>::construct(a, std::addressof(*dest),
                                              std::move(*first));
      std::allocator_traits<Alloc>::destory(a, std::addressof(*first));
      ++first;
      ++dest;
    }
  }

  return dest;
}
