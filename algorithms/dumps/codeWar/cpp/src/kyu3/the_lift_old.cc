#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>

#define DEBUG

enum class Direction { Up, Down };

static Direction direction = Direction::Up;

static int capacity;
static std::vector<int> cabin{};
static std::vector<std::vector<int>>::iterator current_floor;

static void rotate() {
  if (direction == Direction::Up)
    direction = Direction::Down;
  else
    direction = Direction::Up;
}

static void step() { // move in the same direction.
  if (direction == Direction::Up)
    ++current_floor;
  else
    --current_floor;
}

static int acc(int a, std::vector<int> b) { return a + b.size(); };

static int get_floor(const std::vector<std::vector<int>> &queues) {
  std::vector<std::vector<int>>::const_iterator current_floor = ::current_floor;
  return std::distance(queues.cbegin(), current_floor);
}

static auto pick(int floor) {
  return [=](int n) {
    if (cabin.size() >= capacity)
      return false;

    return direction == Direction::Up ? n > floor : n < floor;
  };
}

static int ppl_in_building(const std::vector<std::vector<int>> &queues) {
  return std::accumulate(queues.cbegin(), queues.cend(), 0, acc);
}

auto ppl_ahead_in_building = [](const std::vector<std::vector<int>> &queues,
                                auto fn) {
  std::vector<std::vector<int>>::const_iterator current_floor = ::current_floor;
  int floor = get_floor(queues);

  switch (direction) {
  case Direction::Down:
    return fn(std::make_reverse_iterator(current_floor) + 1, queues.crend(),
              pick(floor), floor);

  case Direction::Up:
  default:
    return fn(current_floor + 1, queues.cend(), pick(floor), floor);
  }
};

static int waiting_ahead_n(const std::vector<std::vector<int>> &queues) {
  int n = ppl_ahead_in_building(
      queues, [](auto first, auto last, auto pick, auto floor) {
        int in_building = std::accumulate(first, last, 0, acc);
        int in_cabin = std::count_if(cabin.cbegin(), cabin.cend(), pick);
        return in_building + in_cabin;
      });
  return n;
}

// pick closest same direction in cabin ,
// pick cloesst same direction in building ahead.
// min of these two.
static int next_stop(const std::vector<std::vector<int>> &queues) {
  return ppl_ahead_in_building(
      queues, [=](auto first_, auto last, auto pick, auto floor) {
        auto first = first_ - 1; // count the current one too.

        std::vector<int> buffer{};
        for (int i = 1; first != last; ++first, ++i) {

          int floor_ = direction == Direction::Up ? floor + i : floor - i;

#ifdef DEBUG
          assert(floor >= 0 && floor < queues.size());
#endif
          // collect ppl want to go in same direction
          if (first->begin() != first->end()) {
            for (auto iter = first->begin(); iter != first->end(); ++iter) {
              int diff = *iter - floor_;

              if (direction == Direction::Up && *iter > floor_ ||
                  direction == Direction::Down && *iter < floor_) {
                buffer.push_back(floor_);
              }
            }
          } else {
          }
        }

        std::copy_if(cabin.cbegin(), cabin.cend(), std::back_inserter(buffer),
                     pick);

        return direction == Direction::Up
                   ? *std::min_element(buffer.begin(), buffer.end()) - floor
                   : floor - *std::max_element(buffer.begin(), buffer.end());
      });
}

// call after current_floor iterator stop at a floor.
void arrive(std::vector<std::vector<int>> &queues) {
  int floor = std::distance(queues.begin(), current_floor);

  // off
  cabin.erase(std::remove(cabin.begin(), cabin.end(), floor), cabin.end());

  // on
  auto pickup = [=]() {
    std::copy_if(current_floor->cbegin(), current_floor->cend(),
                 std::back_inserter(cabin), pick(floor));
    current_floor->erase(std::remove_if(current_floor->begin(),
                                        current_floor->end(), pick(floor)),
                         current_floor->end());
  };
  if (current_floor->size() > 0) {
    int old_cabin_size = cabin.size();
    pickup();

    if (cabin.size() == old_cabin_size) { // nobody get picked up. rotate.
      rotate();
      pickup();
    }
  }
}

static void try_rotate(const std::vector<std::vector<int>> &queues) {
  if (waiting_ahead_n(queues) == 0) {
    rotate();
  }
}

static bool should_back_to_ground(std::vector<std::vector<int>> &queues) {
  return cabin.size() == 0 && ppl_in_building(queues) == 0;
}

void depart(std::vector<std::vector<int>> &queues) {
  if (should_back_to_ground(queues)) {
    direction = Direction::Up;
    current_floor = queues.begin();
    return;
  }

  try_rotate(queues);

  if (cabin.size() == 0) {

    for (; waiting_ahead_n(queues) != 0; step())
      ;
    arrive(queues);

  } else { // cabin not empty

    int floor = get_floor(queues);
    int diff = next_stop(queues);

    for (; diff > 0; --diff, step())
      ;
    arrive(queues);
  }
}

inline static bool running(const std::vector<std::vector<int>> &queues) {
  std::vector<std::vector<int>>::const_iterator current_floor = ::current_floor;
  int floor = std::distance(queues.begin(), current_floor);
  return waiting_ahead_n(queues) != 0 || floor != 0;
}

std::vector<int> the_lift(std::vector<std::vector<int>> queues, int capacity) {
  ::capacity = capacity;
  std::vector<int> lift_log = {};
  current_floor = queues.begin();
  cabin.reserve(capacity);

  do {
    lift_log.push_back(std::distance(queues.begin(), current_floor));
    depart(queues); //@
  } while (running(queues));
  lift_log.push_back(std::distance(queues.begin(), current_floor));

  return lift_log;
}

#define ASSERT_PRINT_(xs)                                                      \
  do {                                                                         \
    std::cout << ": ";                                                         \
    for (auto v : xs) {                                                        \
      std::cout << v << " ";                                                   \
    }                                                                          \
    std::cout << "\n";                                                         \
    assert(xs == result);                                                      \
  } while (0);

int main(void) {
  std::vector<std::vector<int>> queues;
  std::vector<int> result;
  /* queues = {{}, {}, {5, 5, 5}, {}, {}, {}, {}}; */
  /* result = {0, 2, 5, 0}; */
  /* ASSERT_PRINT_(the_lift(queues, 5)); */

  /* queues = {{}, {}, {1, 1}, {}, {}, {}, {}}; */
  /* result = {0, 2, 1, 0}; */
  /* ASSERT_PRINT_(the_lift(queues, 5)); */

  queues = {{}, {}, {}, {}, {2}, {3}, {}};
  result = {0, 5, 4, 3, 2, 1, 0};
  ASSERT_PRINT_(the_lift(queues, 5));
  return 0;
}
