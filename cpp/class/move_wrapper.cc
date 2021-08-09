#include <cassert>
#include <cstring>
#include <iostream>
#include <memory>

//////////////////////////////////////////////////////////////////////////////
// destructive move ptr invalidate the access of old value

template <typename T, typename... Args> class MoveWrapper {
  bool not_moved = true;
  T content_;
  T (*clone_)(void *);
  MoveWrapper(MoveWrapper<T> &to, T *from) {
    std::memcpy(&to.content_, from, sizeof(T));
  }

public:
  MoveWrapper(Args &&...args)
      : content_(T{std::forward<Args>(args)...}),
        clone_([](void *ptr) -> T { return *static_cast<T *>(ptr); }) {}

  T *operator->() {
    assert(not_moved && "access moved object");
    return &content_;
  }

  MoveWrapper clone() {
    assert(not_moved && "access moved object");
    return clone_(&content_);
  }

  MoveWrapper destructive_move(MoveWrapper<T> &to, const char *str = "") {
    assert(not_moved && "access moved object");
    not_moved = false;
    std::cout << "[calling destructive_move]" << str;
    return MoveWrapper(to, &content_);
  }
};

//////////////////////////////////////////////////////////////////////////////
// A non throw Foo
struct Foo {
  int *data, size;
  void report(const char *str) {
    std::cout << "[reporting from " << str << "] < ";
    for (int i = 0; i < size; ++i, std::cout << data[i] << ' ')
      ;
    std::cout << ">\n";
  }

  void fill_8() {
    size = 8, data = new int[8];
    for (int i = 0; i < 8; ++i, data[i] = i)
      ;
  }

  ~Foo() {
    std::cout << "[Foo::~Foo()]" << std::endl;
    delete[] data;
  }

  Foo() : data(nullptr), size(0) {}
  Foo(const Foo &other) : size(other.size), data(new int[other.size]) {
    std::memcpy(data, other.data, sizeof(int) * other.size);
  }

  Foo &operator=(const Foo &other) {
    size = other.size, data = new int[other.size];
    std::memcpy(data, other.data, sizeof(int) * other.size);
    return *this;
  }

  // assume we don't have move semantics.
};

void take_foo(MoveWrapper<Foo> &wfoo) {
  MoveWrapper<Foo> foo1;
  std::cout << "  | ";
  wfoo.destructive_move(foo1, "\n  | "); // move wfoo to foo1
  std::cout << "  | ";                   // wfoo is invalidated
  foo1->report("foo1 in take_foo"); // pointer still points to the same place
  std::cout << "  | [END of take_foo]\n  | ";
}

int test_Foo(void) {                         // 0
  MoveWrapper<Foo> foo;                      // 1 --+
  foo->fill_8();                             // 2   |
  foo->report("foo in main");                // 4 foo is destructively moved.
  take_foo(foo);                             // 3 --+
  foo->report("foo in main");                // 4 foo is destructively moved.
  std::cout << "[END of main]" << std::endl; // 5
  return 0;                                  // 6
}

/////////////////////////////////////////////////////////////////////////////
// A self referential bar
struct Bar {
  int *data, size, *cursor;
  void report(const char *str) {
    std::cout << "[reporting from " << str << "] < ";
    for (int i = 0; i < size; ++i, std::cout << data[i] << ' ')
      ;
    cursor = data + size / 2;
    std::cout << ">\n";
  }

  void fill_8() {
    size = 8, data = new int[8];
    for (int i = 0; i < 8; ++i, data[i] = i)
      ;
  }

  ~Bar() {
    std::cout << "[Foo::~Foo()]" << std::endl;
    delete[] data;
  }

  Bar() : data(nullptr), size(0) {}
  Bar(const Foo &other) : size(other.size), data(new int[other.size]) {
    std::memcpy(data, other.data, sizeof(int) * other.size);
  }

  Bar &operator=(const Foo &other) {
    size = other.size, data = new int[other.size];
    std::memcpy(data, other.data, sizeof(int) * other.size);
    return *this;
  }

  // assume we don't have move semantics.
};

void take_bar(MoveWrapper<Bar> &wfoo) {
  MoveWrapper<Bar> foo1;
  std::cout << "  | ";
  wfoo.destructive_move(foo1, "\n  | "); // move wfoo to foo1
  std::cout << "  | ";                   // wfoo is invalidated
  foo1->report("foo1 in take_foo"); // pointer still points to the same place
  std::cout << "  | [END of take_foo]\n  | ";
}

int test_Bar(void) {                         // 0
  MoveWrapper<Foo> foo;                      // 1 --+
  foo->fill_8();                             // 2   |
  foo->report("foo in main");                // 4 foo is destructively moved.
  take_foo(foo);                             // 3 --+
  foo->report("foo in main");                // 4 foo is destructively moved.
  std::cout << "[END of main]" << std::endl; // 5
  return 0;                                  // 6
}

int main(void) {
  // test_Foo();
  test_Bar();
  return 0;
}
