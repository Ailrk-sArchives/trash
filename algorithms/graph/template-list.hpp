#include <iostream>

struct Nil {};

template <typename T> struct Return { using type = T; };

// varadic ...Ts includes on or more, and it's already recursive by itself.
// there is no need to define a ADT style list.
template <typename... Ts> struct List {
  using type = List<Ts...>;
  static constexpr size_t size = sizeof...(Ts);

  template <typename... U> using concat = List<U..., Ts...>;
};

template <typename...> struct Head {};
template <typename T, typename... Ts> struct Head<List<T, Ts...>> {
  using type = T;
};

template <typename...> struct Tail {};
template <typename T, typename... Ts> struct Tail<List<T, Ts...>> {
  using type = List<Ts...>;
};

// again, c++ unfolding varadic does most of job for most of things...
// you can use it to map over elements based on some statement.
// in meta programming statements in ast is our basic building block.
// it's a powerful irregular tool.
template <template <typename> typename F, typename XS> struct Map {};
template <template <typename> typename F, typename... Ts>
struct Map<F, List<Ts...>> : List<typename F<Ts>::type...> {};

// default type parameter can introduce local binding.
template <template <typename> typename Pred, typename XS,
          typename Base = List<>>
struct Filter : Base {};

template <template <typename> typename Pred, typename Base, typename T,
          typename... Ts>
struct Filter<Pred, Base, List<T, Ts...>> {
  using type = std::conditional_t<
      Pred<T>::value,
      Filter<Pred, typename Base::template concat<T>::type, Ts...>,
      Filter<Pred, Base, Ts...>>;
};

template <template <typename> typename Op, typename I, typename XS>
struct FoldeL : Return<I> {};

template <template <typename> typename Op, typename Acc, typename T,
          typename... Ts>
struct FoldeL<Op, Acc, List<T, Ts...>>
    : FoldeL<Op, typename Op<typename Acc::type>::type, List<Ts...>> {};

template <typename E, typename XS> struct Elem {};
template <typename E, typename... Ts>
struct Elem<E, List<Ts...>> : std::bool_constant<(std::is_same_v<Ts> || ...)> {
};

#define typeof(TYPE) static_assert(sizeof(TYPE) == -1)

// using t = List<int, double, char>;
// typeof(Tail<t>::type);

int main(void) { return 0; }
