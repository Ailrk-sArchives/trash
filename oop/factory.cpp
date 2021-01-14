#include <iostream>
#include <string>

// factory create objects from concrete classes but return
// them as objects of abstarct type or interfaces.

// Point of factory
// 1. Allows you to introduce inversion of control easier.
// 2. Allows you to mock interfaces, which make it eaiser to test.
// 3. To replace a long constructor

// To be honest how many of stataments above acutally holds?
// OOP ppl...

class Product {

public:
  virtual ~Product();
  virtual std::string call_me() const = 0;
};

class ConcreteProduct1 : public Product {
public:
  std::string call_me() const override { return "from ConcreteProduct1"; }
};

class ConcreteProduct2 : public Product {
public:
  std::string call_me() const override { return "from ConcreteProduct2"; }
};

class Factory {

public:
  virtual ~Factory();
  virtual Product *factory_call() const = 0;

  std::string some_operation() const {
    Product *p = this->factory_call();
    std::string result = "factory: " + p->call_me();
    delete p;
    return result;
  }
};

// two concrete factories.
class ConcreteFactory1 : public Factory {
public:
  Product *factory_call() const override { return new ConcreteProduct1(); }
};

class ConcreteFactory2 : public Factory {
public:
  Product *factory_call() const override { return new ConcreteProduct2(); }
};

void client(const Factory &factory) {

  std::cout << "I don't know what factory it is, but I call still call"
               "The function"
            << factory.some_operation() << std::endl;
}

int main(void) {
  Factory *f1 = new ConcreteFactory1();
  Factory *f2 = new ConcreteFactory2();

  client(*f1);
  client(*f2);

  delete f1;
  delete f2;

  return 0;
}
