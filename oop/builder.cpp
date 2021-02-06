#include <iostream>
#include <vector>

// Builder.
//
// It's used when you need to create an object with many possible
// configurations

class Produce1 {
public:
  std::vector<std::string> parts_;
  void ListParts() const {
    std::cout << "Product parts: ";
    for (int i = 0; i < parts_.size(); ++i) {
      if (parts_[i] == parts_.back()) {
        std::cout << parts_[i];
      } else {
        std::cout << parts_[i] << ", ";
      }
    }
    std::cout << "\n" << std::endl;
  }
};

class Builder {
public:
  virtual ~Builder();
  virtual void produceA() const = 0;
  virtual void produceB() const = 0;
  virtual void produceC() const = 0;
};

class ConcreteBuilder : public Builder {
  Produce1 *product;

public:
  ConcreteBuilder() { this->reset(); }
  ~ConcreteBuilder() { delete product; }

  void reset() { this->product = new Produce1(); }

  void produceA() const override { this->product->parts_.push_back("PA"); }
  void produceB() const override { this->product->parts_.push_back("PB"); }
  void produceC() const override { this->product->parts_.push_back("PC"); }

  //
  Produce1 *get_product() {
    Produce1 *reuslt = this->product;
    this->reset();
    return reuslt;
  }
};

// director is just a helper to call builder in a specific order.

class Director {
private:
  Builder *builder;

public:
  void set_builder(Builder *builder) { this->builder = builder; }

  void build_minimal() { this->builder->produceA(); }

  void build_full() {
    this->builder->produceA();
    this->builder->produceB();
    this->builder->produceC();
  }
};

int main(void) { return 0; }
