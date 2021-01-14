#include <iostream>
#include <string>
#include <unordered_map>

// Creational design pattern
// allows cloning objects without coupling
// to their specific class

// The point is to let you copy an existing
// object without making the code depend on
// their classes.

// What's the point?
// 1. Copy a pre configured object is faster than making a new one.
// 2. In c++ class is not first class oject, but prototype allows
//    you to define what kind of object create by the caller at the
//    runtime.

enum Type { PROTOTYPE1 = 0, PROTOTYPE2 };

class Prototype {
protected:
  std::string prototype_name_;
  float prototype_field_;

public:
  Prototype() {}
  Prototype(std::string prototype_name) : prototype_name_(prototype_name) {}

  virtual ~Prototype() {}
  virtual Prototype *clone() const = 0;
  virtual void call_me(float prototype_field) {
    prototype_field_ = prototype_field;
    std::cout << "Call from " << prototype_name_ << " with field "
              << prototype_field_ << std::endl;
  }
};

class ConcretePrototype1 : public Prototype {
private:
  float concrete_prototype_field1_;

public:
  ConcretePrototype1(std::string prototype_name,
                     float concrete_prototype_field1)
      : Prototype(prototype_name),
        concrete_prototype_field1_(concrete_prototype_field1) {}

  // idealy this should return a unique_ptr.
  Prototype *clone() const override { return new ConcretePrototype1(*this); }
};

class ConcretePrototype2 : public Prototype {
private:
  float concrete_prototype_field2_;

public:
  ConcretePrototype2(std::string prototype_name,
                     float concrete_prototype_field2)
      : Prototype(prototype_name),
        concrete_prototype_field2_(concrete_prototype_field2) {}

  // idealy this should return a unique_ptr.
  Prototype *clone() const override { return new ConcretePrototype2(*this); }
};

// Use the PrototypeFactory to clone new object from the
// existing one.
class PrototypeFactory {
private:
  std::unordered_map<Type, Prototype *, std::hash<int>> prototypes_;

public:
  PrototypeFactory() {
    prototypes_[Type::PROTOTYPE1] = new ConcretePrototype1("P1", 1.1);
    prototypes_[Type::PROTOTYPE2] = new ConcretePrototype2("P2", 2.2);
  }

  ~PrototypeFactory() {
    delete prototypes_[Type::PROTOTYPE1];
    delete prototypes_[Type::PROTOTYPE2];
  }

  Prototype *create_prototype(Type type) { return prototypes_[type]->clone(); }
};

void client(PrototypeFactory &factory) {
  std::cout << "create a prototype 1" << std::endl;
  Prototype *p1 = factory.create_prototype(PROTOTYPE1);
  p1->call_me(11.2);
  delete p1;

  std::cout << "\n";
  std::cout << "create a prototype 2" << std::endl;

  Prototype *p2 = factory.create_prototype(PROTOTYPE1);
  p2->call_me(1.2);
  delete p2;
}


int main(void)
{
  PrototypeFactory *factory = new PrototypeFactory();
  client(*factory);
  delete factory;
  return 0;
}
