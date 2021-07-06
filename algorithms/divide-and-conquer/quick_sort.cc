#include <array>
#define BOOST_STACKTRACE_USE_ADDR2LINE

#include <boost/stacktrace.hpp>
#include <iostream>
#include <iterator>
#include <list>
#include <random>
#include <set>
#include <vector>

// #define TEST

// quick sort takes bidirectional iterators.

auto hoare_partition = [](auto start, auto end, auto &comp) {
  typename std::iterator_traits<decltype(start)>::value_type pivot =
      *(start + std::distance(start, end) / 2);

  decltype(start) i = start - 1;
  decltype(start) j = end + 1;

  for (;;) {
    do
      i++;
    while (comp(*i, pivot));
    do
      j--;
    while (!comp(*j, pivot) && *j != pivot);
    if (i >= j)
      break;
    std::swap(*i, *j);
  }
  return j;
};

auto lomuto_partition = [](auto start, auto end, auto &comp) {
  typename std::iterator_traits<decltype(start)>::value_type pivot = *(end - 1);
  decltype(start) i = start;
  for (decltype(start) j = start; j != end; ++j) {
    if (comp(*j, pivot)) {
      std::swap(*i, *j);
      i = i + 1;
    }
  }
  std::swap(*i, *(end - 1));
  return i;
};

// quick sort with custom comparator and partition scheme
template <typename BiIter, typename Comparator, typename Partition>
void quick_sort_impl(BiIter begin, BiIter end, const Partition &partition,
                     const Comparator &comp) {
  static_assert(
      std::is_convertible_v<
          typename std::iterator_traits<decltype(begin)>::iterator_category,
          std::bidirectional_iterator_tag>,
      "quick sort takes bidirectional iterators");

  if (begin >= end)
    return;
  BiIter p = partition(begin, end, comp);
  quick_sort_impl(begin, p, partition, comp);
  quick_sort_impl(p + 1, end, partition, comp);
}

// quick sort interface
template <typename BiIter, typename Comparator, typename Partition>
void quick_sort(BiIter begin, BiIter end, const Partition &partition,
                const Comparator &comp) {
  quick_sort_impl(begin, end - 1, partition, comp);
}

template <typename BiIter, typename Comparator>
void quick_sort(BiIter begin, BiIter end, const Comparator &comp) {
  quick_sort_impl(begin, end - 1, hoare_partition, comp);
}

template <typename C> void print_seq(C &&container) {
  for (auto &n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

///////////////////////////////////////////////////////////////////////////////

// we can sort this thing
struct String {
private:
  char *data_;
  size_t size_;

public:
  String(const std::string &str) {
    std::cout << "copy" << std::endl;
    std::cout << boost::stacktrace::stacktrace() << std::endl;
    data_ = new char[str.size()];
    char *top = data_;
    for (auto &c : str)
      *top++ = c;
    this->size_ = str.size();
  }

  String(const char *str, size_t size) {
    data_ = new char[size];
    char *top = data_;
    for (size_t i = 0; i < size; ++i)
      *top++ = str[i];
    this->size_ = size;
  }

  String(const String &str) : String(str.data_) {}
  String(String &&str) : String(std::move(str.data_)) {}

  ~String() {
    delete[] data_;

#ifdef TEST
    std::cout << "String is destructed" << std::endl;
#endif
  }

  void swap(String &str) {
    char *tmp = this->data_;
    this->data_ = str.data_;
    str.data_ = tmp;
    this->size_ = str.size();
  }

  String &operator=(const String &str) {
    String tmp{str};
    std::swap(data_, tmp.data_);
    return *this;
  }

  String &operator=(const String &&str) {
    String tmp{str};
    std::swap(data_, tmp.data_);
    return *this;
  }

  size_t size() const { return this->size_; }
  char *data() const { return this->data_; }

  friend bool operator<(const String &s1, const String s2) {
    int i;
    for (i = 0;
         i < std::max(s1.size(), s2.size()) && s1.data_[i] == s2.data_[i]; ++i)
      ;
    return s1.data_[i] < s2.data_[i];
  }

  friend bool operator==(const String &s1, const String s2) {
    if (s1.size() != s2.size())
      return false;

    for (int i = 0; i < s1.size(); ++i) {
      if (s1.data_[i] != s2.data_[i]) {
        return false;
      }
    }
    return true;
  }

  friend bool operator<=(const String &s1, const String &s2) {
    return s1 == s2 || s1 < s2;
  }

  friend bool operator>(const String &s1, const String &s2) {
    return !(s1 <= s2);
  }

  friend bool operator>=(const String &s1, const String &s2) {
    return !(s1 < s2);
  }

  friend std::ostream &operator<<(std::ostream &os, const String &str) {
    os << "|";
    for (int i = 0; i < str.size(); ++i) {
      os << str.data()[i];
    }
    os << "|";
    return os;
  }
};

int main(void) {
  {
    std::vector<int> v{5, 2, 4, 9, 1, 3, 8};
    quick_sort(v.begin(), v.end(), [](auto a, auto b) { return a < b; });
    print_seq(v);
  }
  {
    std::array<int, 7> arr{5, 2, 4, 9, 1, 3, 8};
    quick_sort(arr.begin(), arr.end(), [](auto a, auto b) { return a < b; });
    print_seq(arr);
  }

  {
    std::vector<int> v{5, 2, 4, 9, 1, 3, 8};
    quick_sort(v.begin(), v.end(), lomuto_partition,
               [](auto a, auto b) { return a < b; });
    print_seq(v);
  }
  {
    std::array<int, 7> arr{5, 2, 4, 9, 1, 3, 8};
    quick_sort(arr.begin(), arr.end(), lomuto_partition,
               [](auto a, auto b) { return a < b; });
    print_seq(arr);
  }

  {
    std::vector<String> sv{String("zsd"), String("acds"), String("ufnd")};
    quick_sort(sv.begin(), sv.end(), lomuto_partition,
               [](auto a, auto b) { return a < b; });
    print_seq(sv);
  }

  return 0;
}
