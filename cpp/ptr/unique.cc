#include <iostream>
#include <memory>
#include <vector>

// First makes some class with a hierchy.
// This will be allocated on the heap.
class Investment {

public:
  // virutal destructor for derived classes.
  virtual ~Investment() {}

  // avoid slicing
  Investment() = default;
  Investment(const Investment &) = delete;
  Investment &operator=(const Investment &) = delete;

  // define some interfaces.
  virtual size_t amount() const = 0;
};

class Stock : public Investment {

public:
  size_t value = 1000;
  size_t span = 1000;

  Stock(size_t &val, size_t duration) : value(val) {}
  size_t amount() const override { return value; }
};

class Bond : public Investment {
public:
  size_t value = 1000;
  Bond(size_t &val) : value(val) {}
  size_t amount() const override { return value; }
};

class RealEstate : public Investment {

public:
  size_t value = 1000000;

  RealEstate(size_t &val) : value(val) {}
  size_t amount() const override { return value; }
};

enum class MakeWhatInvestment { MakeBond, MakeStock, MakeRealEstate };

// you return the unique pointer of Investment, indicates you transfer the
// ownership of the underlying resouce to the caller.
template <typename... Ts>
std::unique_ptr<Investment> make_investment(MakeWhatInvestment m,
                                            Ts &&... params) {
  // custom destructor for unique pointers.
  auto del = [](Investment *p) {
    std::cout << "die" << std::endl;
    delete p;
  };

  std::unique_ptr<Investment, decltype(del)> p(nullptr, del);

  if (m == MakeWhatInvestment::MakeBond) {
    // forward all parameters to Stock's constructor.
    p.reset(new Stock(std::forward<Ts>(params)...));
    // TODO I dont know why this doesn't work...
  }


  return p;
}

void investment_balckhold(std::unique_ptr<Investment> p){
    // take away your investment.
};

void investment_balckhold_uref(std::unique_ptr<Investment> &&p) {}

int main(void) {

  // now p_investment owns the unverlying Investment.
  auto p_investment =
      make_investment(MakeWhatInvestment::MakeBond, 10, 20, "asd",
                      std::string("ssd"), std::vector<int>{1, 2, 3});

  // this is not allowed because passing by value will copy the unique_ptr,
  // but the coopy constructor of a unique ptr is deleted.
  // investment_balckhold(p_investment);

  // this is ok. p_investment now it nullptr
  // and the ownership is transferred into investment_balckhold.
  // because it's am empty function, one the execution hit out of the scope,
  // the investment will be destroyed.
  investment_balckhold(std::move(p_investment));

  // here we create a new uique pointer and transfer the ownership to
  // p_investment again.
  p_investment = make_investment(MakeWhatInvestment::MakeStock, 10, 20, "asd",
                                 std::string("ssd"), std::vector<int>{1, 2, 3});

  // you still need to convert to to rvale.
  // universal reference doesnt take by value.
  investment_balckhold_uref(std::move(p_investment));

  return 0;
}
