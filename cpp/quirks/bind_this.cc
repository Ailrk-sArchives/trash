#include <functional>
#include <iostream>
#include <vector>

struct Foo {
    void do_a() { std::cout << "a" << std::endl; }
    void do_b() { std::cout << "b" << std::endl; }
};

template <typename Event> struct C {
    using Callback = std::function<void()>;
    size_t current_id = 0;
    std::vector<Callback> listeners{};

    size_t bind(Callback &&callback) {
        listeners.push_back(callback);
        return current_id++;
    }

    template <typename Func, typename T>
    decltype(auto) bind(Func func, T this_) {
        return bind(
            [func, this_] { std::apply(func, std::make_tuple(this_)); });
    }
    void emit_all() {
        for (auto &n : listeners) n();
    }
};

int main(void) {
    Foo foo;
    C<Foo> c;
    c.bind(&Foo::do_a, foo);
    c.bind(&Foo::do_b, foo);
    c.bind(&Foo::do_b, foo);
    c.bind(&Foo::do_b, foo);
    c.bind(&Foo::do_b, foo);
    c.bind(&Foo::do_b, foo);
    c.emit_all();
    return 0;
}
