#include <algorithm>
#include <iostream>
#include <memory>
#include <optional>
#include <type_traits>

// https://www.foonathan.net/2017/09/destructive-move/#content
// https://radekvit.medium.com/move-semantics-in-c-and-rust-the-case-for-destructive-moves-d816891c354b
//////////////////////////////////////////////////////////////////////////////
// Non destructive Move (C++):
//    Move from object still usable, just in a invalid state.
// Destructive Move (Rust):
//    Move from object just gone.
//
// For example, In c++ lots of moves are implemneted with swap, simply swap
// the element between two objects. Why do you want to swap? Isn't it
// expensive to create a new temporary?
// Because c++ has exception and you have to deal with throwable Constructors
// and destructors.

//////////////////////////////////////////////////////////////////////////////
// problem: Throwable move operations

template <typename T> struct Vec {
private:
  T *data_;
  size_t size_;
  size_t capacity_;

public:
  ~Vec() {
    for (int i = 0; i < size_; ++i) {
      delete data_[i];
    }
  }

  Vec() : data_(new T[0]), size_(0), capacity_(64) {}

  // a simple example of how to handle types with different semantics.
  // given a type, we need to think about:
  // 1. is the type trivially copyable?
  //    - if is, we probably can go ahead with memcpy
  // 2. is it movable?
  //    - if is, we probably can go ahead with memcpy.
  // 3. does it throw?
  //    - if is, we need to deal with the throwing case.
  Vec(Vec const &other)
      : size_(other.size_), capacity_(other.capacity_),
        data_(new T[capacity_]) {
    if constexpr (std::is_trivially_copy_constructible_v<T>) {
      std::uninitialized_copy(std::begin(other.data_), std::end(other.data_),
                              std::begin(data_));
    } else if constexpr (std::is_nothrow_copy_assignable_v<T> &&
                         std::is_nothrow_copy_assignable_v<T>) {
      for (int i = 0; i < size_; ++i) {
        *data_[i] = *other.data_[i];
      }
    } else if constexpr (std::is_copy_constructible_v<T> &&
                         std::is_copy_assignable_v<T>) {
      int i = 0;
      try {
        for (; i < size_; ++i) {
          *data_[i] = *other.data_[i];
        }
      } catch (...) {
        for (int j = 0; j < i; ++j) {
          delete data_[j];
        }
      }
    }
  }

  Vec &operator=(Vec const &other) {
    auto tmp = Vec{other};
    std::swap(*this, tmp);
  }

  // well this is destructive move.
  // move simply takes the pointer of data from other, and invalidate the old
  // ptr. Nothing really get moved, and it's a farily trivial operation.
  // this move is exception safe, we guarantee no throw.
  //
  // It's really nice to mark it as noexcept because now this Vec type can
  //  itself be deteceted and handle differently.
  Vec(Vec &&other) noexcept : size_(other.size_), capacity_(other.capacity_) {
    data_ = other.data_;
    other.data_ = nullptr;
  }

  Vec &operator=(Vec &&other) noexcept {
    auto tmp = std::move_if_noexcept(other);
    std::swap(this, tmp);
  }
};

//////////////////////////////////////////////////////////////////////////////
// be careful with pointer semantics
// unique ptr. We don't want it to be copyable.
//
// When writing move, there are two entities involves:
//  1. a move from object
//  2. a move to object
//
// After move, the state of a move from object should be in a valid but
// unspecified state, and move to object should be the new move from.
//
// The move from state is the special case that you want to handle.
//
// Here, the intention of a unique ptr is to have an uniqeuly owned not null
// ptr, but the move introduce a move from state that the move from object
// will contain a nullptr.
// 67

template <typename T> struct OwningPtr {
  T *ptr_;

  template <typename... Args>
  explicit OwningPtr(Args &&...args)
      : ptr_(new T{std::forward<Args>(args)...}) {}

  // avoid double free.
  ~OwningPtr() {
    if (ptr_)
      delete ptr_;
  }

  OwningPtr(const OwningPtr &) = delete;
  OwningPtr &operator=(const OwningPtr &) = delete;

  // move?
  OwningPtr(OwningPtr &&other) : ptr_(other.ptr_) { other.ptr_ = nullptr; }
  OwningPtr &operator=(OwningPtr &&other) {
    ptr_ = other.ptr_;
    other.ptr_ = nullptr;
  }

  // the object can be in moved from state, and in that state these two
  // operations are invalid.
  T &operator*() { return *ptr_; }
  T *operator->() { return ptr_; }
};

static_assert(std::is_move_constructible_v<OwningPtr<int>>);

// is there a way that we can forbid ppl to use an object in a moved from state?

//////////////////////////////////////////////////////////////////////////////
// think about the life time of an object as a state machine, in different
// states different oerations are valid.

//////////////////////////////////////////////////////////////////////////////
// The tricky part of handling destructive move is to avoid pointers in move
// from state and move to object share the same heap memory. Once we
// adjust that case, usually there will be no other cases that make the move
// throw.
//
// So destructive move generally are nothrow.

template <typename T> struct List {

  // a node owns it's content.
  struct Node {
    std::unique_ptr<T> content_;
    T *next;

    static std::unique_ptr<Node> make_sentinel() {
      return std::make_unique<Node>({nullptr, nullptr});
    }

    bool is_sentinel() const { return content_ == nullptr && next == nullptr; }
  };

  static_assert(std::is_copy_constructible_v<Node>);

  std::unique_ptr<Node> node;

  List() : node(std::move(Node::make_sentinel())) {}
  List(List const &list) {
    // TODO
  }
};
