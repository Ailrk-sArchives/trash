#include <iostream>
#include <memory>

class VisitableBase;

class VisitorBaseInternal {
public:
  virtual void visit(VisitableBase &) = 0;
  virtual ~VisitorBaseInternal() {}
};

class VisitableBase {
  virtual void accept(VisitorBaseInternal &visitor) { visitor.visit(*this); }
};

template <typename T> class VisitorBase : public VisitorBaseInternal {
  void visit(VisitableBase &e);
};

