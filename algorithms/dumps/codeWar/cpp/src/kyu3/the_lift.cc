#define ASSERT_PRINT_(xs)                                                      \
  do {                                                                         \
    std::cout << ": ";                                                         \
    for (auto v : xs) {                                                        \
      std::cout << v << " ";                                                   \
    }                                                                          \
    std::cout << "\n";                                                         \
    assert(xs == result);                                                      \
  } while (0);

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

namespace util {
auto acc = [](int a, auto b) { return a + b.size(); };

}

static int capacity;
static std::vector<int> log{};
static std::vector<std::vector<int>> queues;
static std::vector<int> cabin{};
static bool is_up = true;
static decltype(queues)::iterator current;

auto get_range() {
  auto first = is_up ? current + 1 : queues.begin();
  auto last = is_up ? queues.end() : current - 1;
  return std::make_tuple(first, last);
}

int get_floor() { return std::distance(queues.begin(), current); }

void rotate() { is_up = !is_up; }

// step towards the direction
void step() {
  if (is_up) {
    current++;
  } else {
    current--;
  }
}

bool is_same_direction(int src, int dest) {
  if (cabin.size() == 0) {
    return false;
  }

  if (is_up) {
    return dest > src;
  } else {
    return dest < src;
  }
}

void back_to_ground() {
  current = queues.begin();
  is_up = true;
  log.push_back(get_floor());
}

// deliver ppl
void offboard() {
  int floor = get_floor();
  auto it = std::remove(cabin.begin(), cabin.end(), floor);
  cabin.erase(it, cabin.end());
}

// ppl up
void onboard() {
  int floor = get_floor();
  auto first = current->begin();
  auto last = current->end();
  auto pred = [=](auto n) { return is_same_direction(floor, n); };

  std::copy_if(first, last, std::back_inserter(cabin), pred);

  auto iter = std::remove_if(first, last, pred);
  current->erase(iter, current->end());
}

// people waiting ahead to on/off
int waiting_along_n() {
  int in_cabin = cabin.size();
  auto [first, last] = get_range();

  int in_building = std::accumulate(first, last, 0, util::acc);

  return in_building + in_cabin;
}

int waiting_n() {
  int n = std::accumulate(queues.begin(), queues.end(), 0, util::acc);
  return n;
}

bool is_finished() {
  bool cabin_empty = cabin.size() == 0;
  bool building_empty = waiting_n() == 0;
  return cabin_empty && building_empty;
}

// step till the other side
int step_across() {
  for (; waiting_along_n() != 0; step())
    ;
  return get_floor();
}

// step till next need
int step_n(int n) {
  for (int i = 0; i < n; ++i) {
    step();
  }
  return get_floor();
}

// the
typename std::iterator_traits<decltype(current)>::difference_type
shortest_next_hop() {
  int floor = get_floor();
  std::vector<int> buffer{};

  auto pred = [=](int n) {
    return (is_up && n > floor) || (!is_up && n < floor);
  };

shortest_cabin : {
  auto first = cabin.begin();
  auto last = cabin.end();
  std::copy_if(first, last, std::back_inserter(buffer), pred);
}

shortest_building : {
  auto [first, last] = get_range();
  for (; first != last; ++first) {
    auto floor = std::distance(queues.begin(), first);
    for (auto v : *first) {
      if (pred(v)) {
        buffer.push_back(floor);
        continue;
      }
    }
  }
}

  auto bfirst = buffer.begin();
  auto bend = buffer.end();
  return is_up ? *std::min_element(bfirst, bend) - floor
               : floor - *std::max_element(bfirst, bend);
}

// find next floor
void advance() {
  if (is_finished()) {
    current = queues.begin();
    is_up = true;
    return;
  }

  if (waiting_along_n() == 0) {
    rotate();
  }

  if (cabin.size() == 0) {
    step_across();

  } else {
    auto n = shortest_next_hop();
    step_n(n);
  }
}

std::vector<int> the_lift(std::vector<std::vector<int>> queues, int capacity) {
  ::queues = queues;
  ::capacity = capacity;
  cabin.reserve(capacity);
  current = queues.begin();

  while (!is_finished()) {
    offboard();
    onboard();
    advance();
  }

  return log;
}

int main(void) {
  std::vector<std::vector<int>> queues;
  std::vector<int> result;
  // queues = {{}, {}, {5, 5, 5}, {}, {}, {}, {}};
  queues = {{2}, {3}, {5, 5, 5}, {}, {}, {}, {4}};
  result = {0, 2, 5, 0};
  ASSERT_PRINT_(the_lift(queues, 5));

  /* queues = {{}, {}, {1, 1}, {}, {}, {}, {}}; */
  /* result = {0, 2, 1, 0}; */
  /* ASSERT_PRINT_(the_lift(queues, 5)); */

  /* queues = {{}, {}, {}, {}, {2}, {3}, {}}; */
  /* result = {0, 5, 4, 3, 2, 1, 0}; */
  /* ASSERT_PRINT_(the_lift(queues, 5)); */

  return 0;
}
