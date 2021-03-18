#include <array>
#include <iostream>
#include <map>
#include <memory>

#define NOLEAK

// try use g++ dict.cc -std=c++17 -fsanitizer=address -g
// and valgrind to track down memeory corruptions.

// or use gdb to step down the constructor to see when stuffs are freed.

// basic raii with new and delete in c++ 03 style.
// notice the rule of five. If we only implement destructor but no copy
// constructor, we will have a double free easily when we try to construct a
// vector of SubValue with initializer list (initializer list construct
// SubValue, copy, and destruct. at this point data is freed. Next time the
// container calles the destructor it will be a double free.)
struct SubValue {
  char *data;
  int len;

  SubValue(const SubValue &s) : len(s.len), data(new char[s.len]) {
    for (int i = 0; i < len; ++i) {
      data[i] = s.data[i];
    }
  }

  SubValue(SubValue &&s) : len(s.len), data(s.data) { s.data = nullptr; }

  // when writing constructors we also care about exception guarantees.
  // exception guarantees can be summarized into 4 categories:
  //  0. no exception
  //  1. strong exception guarantees      -- no side effect on fail
  //  2. basic exception guaramtees       -- no memory leak.
  //  3. exception unsafe

  // this requires us to allocate extra memories, but guarantee strong exception
  // safty.
  // During creation of the copy if anything happen, we fail to copy, but the
  // orignal data is preserved.
  SubValue &operator=(const SubValue &sub) {
    SubValue tmp{sub};
    if (&sub != this) {
      tmp.swap(*this);
    }
    return *this;
  }

  // we can do something similar for move construtor.
  SubValue &operator=(SubValue &&sub) {
    SubValue tmp{std::move(sub)};
    tmp.swap(*this);
    return *this;
  }

  SubValue(char *data, int len) : data(data), len(len) {}

  ~SubValue() {
    std::cout << "Subvalue: yoho I'm starting to freed >>>" << std::endl;

    // check null to avoid double free.
    if (data != nullptr) {
      delete[] data;
    }
    std::cout << "Subvalue: yoho I'm freed <<<" << std::endl;
  }

  void swap(SubValue &sub) { std::swap(data, sub.data); }
};

struct Value {
  SubValue sub;

  // this should be save because we just print stuffs.
  ~Value() { std::cout << "Value: hoyo I'm freed!" << std::endl; }
};

///////////////////////////////////////////////////////////////////////

int main(void) {

  std::cout << "Value ================" << std::endl;

  {
    // freed . when hit } destructor will be called.
    Value v{SubValue{new char[10], 10}};
  }

  ///////////////////////////////////////////////////////////////////////

  std::cout << "Subvalue ================" << std::endl;

  {
    // needs to free.
    auto *vv = new SubValue{new char[10], 10};
#ifdef NOLEAK
    delete vv;
#endif
  }

  ///////////////////////////////////////////////////////////////////////

  std::cout << "Map with boxed value ================" << std::endl;

  {

    std::map<int, Value *> m{

        {1, new Value{SubValue{new char[10], 10}}},
        {2, new Value{SubValue{new char[10], 10}}}

    };

    std::cout << "char is " << m[1]->sub.data[0] << std::endl;

#ifdef LEAK
    // memory leak!
    // clear only clear boxed value. If map holds a pointer it will not free the
    // memory.
    m.clear();
#endif

#ifdef NOLEAK
    for (auto &[k, v] : m) { // don't store boxed value in map.
      delete v;
    }
    m.clear();
#endif
  }

  ///////////////////////////////////////////////////////////////////////

  std::cout << "Map with unboxed value ================" << std::endl;

  {

    std::map<int, Value> m;

    // double free
#ifdef LEAK
    m.emplace(std::make_pair(1, Value{SubValue{new char[10], 10}}));
#endif

#ifdef NOLEAK
    m.emplace(1, Value{SubValue{new char[10], 10}});
#endif
  }

  std::cout << "================" << std::endl;
  return 0;
}
