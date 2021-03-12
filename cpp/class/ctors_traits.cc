#include <memory>
#include <type_traits>

struct trivial_vector3 {
  int x;
  int y;
  int z;
};

// a trival class is a pod.
static_assert(std::is_trivial_v<trivial_vector3>);
static_assert(
    std::is_trivially_assignable<trivial_vector3, trivial_vector3>::value);

struct copyable {
  copyable(const copyable &) = default;
};

static_assert(!std::is_trivial_v<copyable>);
static_assert(std::is_trivially_copy_assignable_v<copyable>);
static_assert(std::is_trivially_move_assignable_v<copyable>);
static_assert(std::is_move_assignable_v<copyable>);
static_assert(std::is_copy_assignable_v<copyable>);
static_assert(std::is_destructible_v<copyable>);
static_assert(std::is_scalar_v<int>);

struct nonmoveable {
  nonmoveable(const nonmoveable &) = default;
  nonmoveable(nonmoveable &&) = delete;
};

// classes has custom ctors are nolong trival
static_assert(!std::is_trivial_v<nonmoveable>);
static_assert(std::is_copy_constructible_v<nonmoveable>);
static_assert(!std::is_move_constructible_v<nonmoveable>);

struct has_nonmovable {
  has_nonmovable(const has_nonmovable &) = default;

  // this will be copy
  has_nonmovable(has_nonmovable &&) = default;
};
static_assert(std::is_copy_constructible_v<has_nonmovable>);
static_assert(std::is_move_constructible_v<has_nonmovable>);
