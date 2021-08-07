#include <any>
#include <iostream>
#include <vector>

// value semantics instead of move semantics for inheritance


// bascially we just a vtable.

struct ICalculator {
  virtual double compute(int) const = 0;
  virtual void log(int, int) const = 0;
  virtual ~ICalculator() {}
};

struct Calculator {
public:

  // erase the type C and store it in any to get dynamic semantics.
  // use the acompany get to cast the value back to the original type.
  // what C is doesn't matter, but we need to cast it back to the same type.
  template <typename C>
  Calculator(C &&calculator)
      : storage{std::forward<C>(calculator)}, get{[](std::any &storage)
                                                      -> ICalculator & {
          return std::any_cast<C &>(storage);
        }} {}

  ICalculator *operator->() { return &get(storage); }

private:
  std::any storage;
  ICalculator &(*get)(std::any &);
};

struct BigCalculator : ICalculator {
  double compute(int input) const override { return input * 5; }

  void log(int input, int output) const override {
    std::cout << "Big cal input: " << input << " output: " << output
              << std::endl;
  }
};

struct SmallCalculator : ICalculator {
  double compute(int input) const override { return input + 1; }

  void log(int input, int output) const override {
    std::cout << "Small cal input: " << input << " output: " << output
              << std::endl;
  }
};

int main(void) {
  std::vector<Calculator> cals;

  cals.push_back(BigCalculator());
  cals.push_back(SmallCalculator());
  cals.push_back(BigCalculator());

  for (auto &n : cals) {
    auto v = n->compute(10);
    n->log(10, v);
  }
  return 0;
}
