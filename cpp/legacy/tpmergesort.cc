// cpp template metaprogramming.
// template <typename a> is like forall a. but not quite.
// Remind merge sort
// 1. divide list into n sublists each with len 1.
// 2. Recursively merge 2 sublists until there is only 1 list remain.

// define list.
struct Nil {}; // a empty type.
// define structure of List.
// value needs to be defined as constexpr,
// type can just be bind with a name with using.
template <int a, typename L> struct List {
  static constexpr int head = a;
  using tail = L;
};

// define Pair.
// it accomodates two types.
template <typename A, typename B> struct Pair {
  using first = A;
  using second = B;
};

// define conditional.
// it can be thought as an if expression which the value is returned via
// `test` binding.
// if_<true, A, B>::test will call the expression and get the return value.
template <bool C, typename Then, typename Else> struct if_ {};
template <typename Then, typename Else> struct if_<false, Then, Else> {
  using test = Then;
};
template <typename Then, typename Else> struct if_<true, Then, Else> {
  using test = Else;
};

// define split func
// split will split List<int a, typename L> into several
// List<int a, Nil>
//
// base case.
template <typename L> struct Split {};

// ues template <> when matching with concrete value
template <> struct Split<Nil> { using type = Pair<Nil, Nil>; };

// a is polymorhpic parameter thus need a forall to specify.
template <int a> struct Split<List<a, Nil>> {
  using type = Pair<List<a, Nil>, Nil>;
};
template <int a, int b, typename Tail> struct Split<List<a, List<b, Tail>>> {
private:
  using _Split_rec = typename Split<Tail>::type;

public:
  using type = Pair<List<a, typename _Split_rec::first>,
                    List<b, typename _Split_rec::second>>;
};

// merge
template <template <int, int> typename P, typename L1, typename L2>
struct Merge {};

template <template <int, int> typename P, typename L2>
struct Merge<P, Nil, L2> {
  using type = L2;
};

template <template <int, int> typename P, typename L1>
struct Merge<P, L1, Nil> {
  using type = L1;
};

template <template <int, int> typename P, int A1, typename Tail1, int A2,
          typename Tail2>
struct Merge<P, List<A1, Tail1>, List<A2, Tail2>> {
  using type = typename if_<
      P<A1, A2>::value,
      List<A1, typename Merge<P, Tail1, List<A2, Tail2>>::type>,
      List<A2, typename Merge<P, List<A2, Tail1>, Tail2>::type>>::test;
};

// sort
template <template <int, int> typename P, typename L> struct Sort {
private:
  using _split_list = typename Split<L>::type;
  using _l1 = Sort<P, typename _split_list::first>;
  using _l2 = Sort<P, typename _split_list::second>;

public:
  using type = typename Merge<P, _l1, _l2>::type;
};

template <template <int, int> typename P> struct Sort<P, Nil> {
  using type = Nil;
};

template <template <int, int> typename P, int a> struct Sort<P, List<a, Nil>> {
  using type = List<a, Nil>;
};
