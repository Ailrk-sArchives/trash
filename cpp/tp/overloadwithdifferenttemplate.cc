#include "lambda_trait.h"
#include <any>
#include <functional>
#include <iostream>
#include <type_traits>

template <typename T> struct F {
    T value;
    using value_type = T;

    // template <typename U> F<U> map(std::function<U(T)>);

    template <typename Fn, typename U = typename lambda_traits<Fn>::return_type,
              typename T1 = typename lambda_traits<Fn>::template parameter<0>>
    F<U> map(Fn fn);
};

// we want to infer both U and T. T is from the caller U is from the function.
// if we type erase and deduce U, we can't specify T. If we specify T with
// std::function we can't deduce U (instead we need to provided U.)

template <typename T>
template <typename Fn, typename U, typename T1>
F<U> F<T>::map(Fn fn) {
    static_assert(std::is_same_v<T1, T>);

    U u = static_cast<std::function<U(T)>>(fn)(value);
    return F<U>{ u };
}

int main(void) {
    F<int> f{ 1 };
    F<double> f1 = f.map([](int a) { return 1.1; });

    return 0;
}
