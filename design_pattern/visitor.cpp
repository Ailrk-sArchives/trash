#include <iostream>
#include <string>

class ConcreteComponentA;
class ConcreteComponentB;

// visitor interface declare a set of methods that corresponds to
// each concrete component classes.
// The problem of this pattern is you need to have a interface
// for each concrete class you gonna work with.
class Visitor {
public:
  virtual void visit_concreteA(const ConcreteComponentA *element);
  virtual void visit_concreteB(const ConcreteComponentB *element);
};

class Component {
public:
  virtual ~Component() {}
  virtual void accept(Visitor *visitor) const = 0;
};

