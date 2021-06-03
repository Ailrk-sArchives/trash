#include <algorithm>
#include <iostream>
#include <list>
#include <string>

// Base class
// Leaf and composite usually implement the same interface.
// Composite can manage it's childs.
// Leafs only do the job
class Component {
protected:
  Component *parent_;

public:
  virtual ~Component() {}
  void set_parent(Component *parent) { parent_ = parent; }

  Component *get_parent() const { return parent_; }

  // define some child management oerations
  virtual void add(Component *component) {}
  virtual void remove(Component *component) {}

  virtual bool is_composite() const { return false; }

  virtual std::string operation() const = 0;
};

// leaf object that do the actual work.
// leaf has no add and remove, thus it cannot control the
// shape of the tree.
class Leaf : public Component {
public:
  std::string operation() const override { return "leaf"; }
};

// Composite class represent the complex components.
// Usually composite class delegate acutal work to their child.
class Composite : public Component {
protected:
  std::list<Component *> childern_;

public:
  void add(Component *c) override {
    childern_.push_back(c);
    c->set_parent(this);
  }

  void remove(Component *c) override {
    childern_.remove(c);
    c->set_parent(nullptr);
  }

  // this is kinda weird.
  // You need this to differentiate composite from
  // leaves.
  // How sad.
  bool is_composite() const override { return true; }

  // composite use operation
  std::string operation() const override {
    std::string result;

    for (const auto *c : childern_) {
      if (c == childern_.back()) {
        result += c->operation();
      } else {
        result += c->operation() + "+";
      }
    }
    return "Branch(" + result + ")";
  }
};

void client1(Component *c) {
  std::cout << "result: " << c->operation() << std::endl;
}

void client2(Component *c1, Component *c2) {
  if (c1->is_composite()) {
    c1->add(c2);
  }
  std::cout << "result: " << c1->operation() << std::endl;
}

int main(void) {
  Component *simple = new Leaf();
  std::cout << "client: a simple component" << std::endl;
  client1(simple);
  std::cout << "\n" << std::endl;

  Component *tree = new Composite();
  Component *branch1 = new Composite();
  Component *branch2 = new Composite();

  Component *leaf1 = new Leaf();
  Component *leaf2 = new Leaf();
  Component *leaf3 = new Leaf();

  branch1->add(leaf1);
  branch1->add(leaf2);
  branch2->add(leaf3);

  tree->add(branch1);
  tree->add(branch2);
  std::cout << "client: Now I have a tree: " << std::endl;
  client1(tree);
  std::cout << "\n" << std::endl;

  std::cout << "client: manage the tree" << std::endl;
  client2(tree, simple);

  delete leaf1;
  delete leaf2;
  delete leaf3;
  delete branch1;
  delete branch2;
  delete tree;
  delete simple;
  return 0;
}
