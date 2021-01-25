#include <type_traits>

template <typename T> using Dummy = T;
template <typename T> using List_t = Dummy<T>;
template <typename T> using Set_t = Dummy<T>;

template <typename T, typename A, typename B, typename Res> struct Replace;
template <typename T, typename A> struct Replace<T, A, A, T>;

/* template <typename T, typename A, typename B, typename Res> */
/* struct Replace<std::set<T>, A, B, std::set<Res>>; */
/* template <typename T, typename A, typename B, typename Res> */
/* struct Replace<std::set<T>, A, B, std::set<Res>>; */

