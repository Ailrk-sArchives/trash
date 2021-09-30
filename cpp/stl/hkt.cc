#include <iostream>

template <typename T> struct Maybe {
    T value;
    bool nothing;

  public:
    Maybe(const T &v)
        : value(v) {}
    Maybe()
        : nothing(true) {}
};

template <template <typename> typename F> struct Functor {};

template <> struct Functor<Maybe> {
    template <typename A, typename TF>
    static Maybe<typename std::result_of<TF(A)>::type> fmap(const Maybe<A> &x,
                                                            TF func) {
        using Result = typename std::result_of<TF(A)>::type;
        if (x.nothing)
            return Maybe<Result>();
        else
            return Maybe<Result>(func(x.value));
    }
};

template <template <typename> typename H, typename S>
void f(const H<S> &value) {}

template <template <typename, typename> typename V, typename A, typename B>
void g(const V<A, B>);
