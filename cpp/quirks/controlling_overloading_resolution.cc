#include <cstdlib>
#include <iostream>
#include <new>
#include <random>
#include <type_traits>
#include <vector>

///////////////////////////////////////////////////////////////////////////////
// 1. delete
// we can delete an overload for a function.
void func1(int) { std::cout << "int" << std::endl; }
void func1(double) = delete;
void func1(float) = delete;
void func1(long double) = delete;

void test1() {
    func1(5);
    // nonono
    // func1(5.5);
}

// or use sfinae

template <typename T> void func2(T);

template <typename T,
          typename std::enable_if_t<std::is_floating_point<T>::value>>
void func2(T) = delete;

template <> void func2(int) { std::cout << "int" << std::endl; }

void test2() {
    func2(1);
    // func2((double)2.2);
}

///////////////////////////////////////////////////////////////////////////////
// prohibiting implicit conversions for tempories
void func3(const std::string &str);
void func3(const char *) = delete;

void test3() {
    // func3("no implicit conversion");
    func3(std::string("explicit"));
}

///////////////////////////////////////////////////////////////////////////////
// avoid rvalue binds to const reference
//
template <typename T> void func4(T const &obj) {
    std::cout << "yes" << std::endl;
}
template <typename T> void func4(T const &&) = delete;

void test4() {
    // no
    // func4(1);

    int const a = 10;
    func4(a);
}

///////////////////////////////////////////////////////////////////////////////
// constumize error messages

template <typename T> void func5(T const &obj) {
    std::cout << "yes" << std::endl;
}
template <typename T> void func5(T const &&) {
    static_assert(false, "should never call with a temporary");
}

///////////////////////////////////////////////////////////////////////////////
// tag disptaching

// say we want to initialize a piece of uninitialized memory.
// this is not exception safe. once T() throws exceptions memory in *being is
// screwed.
template <typename T> void construct_0(T *begin, T *end) {
    for (auto cur = begin; cur != end; ++cur) {
        ::new (static_cast<void *>(cur)) T();
    }
}

// this will destruct the memory if T() throws.
template <typename T> void construct_1(T *begin, T *end) {
    auto cur = begin;
    try {
        for (; cur != end; ++cur) {
            ::new (static_cast<void *>(cur)) T();
        }
    } catch (...) {
        for (auto p = begin; p != cur; ++p) {
            p->~T();
            throw;
        }
    }
}

// or maybe we don't want a throwable type T at all?

template <typename T> void construct(std::true_type, T *begin, T *end) {
    construct_0(begin, end);
}
template <typename T> void construct(std::false_type, T *begin, T *end) {
    construct_1(begin, end);
}

void test_construct() {
    int *begin = (int *)malloc(1024 * sizeof(int));
    int *end = begin + 1024;

    // wow a compile time proove.
    construct(std::conjunction<std::is_nothrow_default_constructible<int>,
                               std::is_trivially_default_constructible<int>>{},
              begin, end);
}

template <int N> struct priority_tag : priority_tag<N - 1> {};
template <> struct priority_tag<0> {};
static_assert(std::is_base_of_v<priority_tag<0>, priority_tag<1>>);

// add more hireachy for dispatching.
struct default_ctor {};
struct nothrow_default_ctor : default_ctor {};
struct trivial_default_ctor : nothrow_default_ctor {};

///////////////////////////////////////////////////////////////////////////////
// or just sfinae?
template <typename T,
          typename std::enable_if<
              std::is_nothrow_default_constructible<T>::value, int>::type = 0>
void construct_sfinae(T *begin, T *end) {
    for (auto cur = begin; cur != end; ++cur)
        ::new (static_cast<void *>(cur)) T();
}

template <typename T,
          typename std::enable_if<
              !std::is_nothrow_default_constructible<T>::value, int>::type = 0>
void construct_sfinae(T *begin, T *end) {
    auto cur = begin;
    try {
        for (; cur != end; ++cur) ::new (static_cast<void *>(cur)) T();
    } catch (...) {
        for (auto new_cur = begin; new_cur != cur; ++new_cur) new_cur->~T();
        throw;
    }
}
static_assert(std::is_nothrow_default_constructible_v<int>);

struct ThrowDefaultConstructbleFoo {
    int n;
    ThrowDefaultConstructbleFoo() {
        n = random() % 100;
        if (n > 90)
            throw;
    }
};

static_assert(
    !std::is_nothrow_default_constructible_v<ThrowDefaultConstructbleFoo>);

void test_construct_sfinae() {
    {

        // use the nothrow default constructible overload.
        int *begin = (int *)malloc(1024 * sizeof(int));
        int *end = begin + 1024;
        construct_sfinae(begin, end);
    }

    {
        // use the other overload, may throw.
        ThrowDefaultConstructbleFoo *begin1 =
            (ThrowDefaultConstructbleFoo *)malloc(
                1024 * sizeof(ThrowDefaultConstructbleFoo));
        ThrowDefaultConstructbleFoo *end1 = begin1 + 1024;
        construct_sfinae(begin1, end1);
    }
}

// conclusion:
//
// 1. overload can be achieved in several ways:
//    - normal function overloading
//    - sfinae
// 2. we can remove a overload with =delete (or sfinae)
// 3. we can use type traits to choose the right overload for the right type.
// 4. we can use tag disptach to choose overload explicitly.
// 5. deleting overload to prevent unwanted implicit conversion.
