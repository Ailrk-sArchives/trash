#include <cmath>
#include <iostream>

// Complex is a POD,
// plain old data.
// Member variables declared alter in a
// structut must be at a higher address.
struct Complex {
  double real; // 0x80
  double img;  // 0x88
};

// Inheritance extend the object memory
// with new fields.
// Note if it's inherited is no longer POD.
struct Derived : Complex {
  double angle;
  /* data */
};

// Derived has similar layout as this.
struct LayotOfDerived {
  struct {
    double real;
    double img;
  };
  double angle;
};

// Now complex has a method
// But it's still a pod.
// the method resides somewhere else, and it
// only needs an address
struct Complex1 {
  double real; // 0x80
  double img;  // 0x88

  double abs() const { return std::hypot(real, img); }
};

// abs member function will be compiled into a separate function like this.
// compiler generated functions will have a mangled name,
// this happens for both methods and function overload.
// use c++filt to reverse the mangled name.
double _ZNK7Complex1absEv(Complex1 const *self) {
  return std::hypot(self->real, self->img);
}

// What about class with polymorphism
// Non virtual fuction find statically.
// virtual function binds dynamically with virtual table.
// --------
//  Erdos
// --------
//  vtable -> .....-> who_am_i_really() {}
struct Erdos {
  void who_am_i() { std::cout << "Erdos" << std::endl; }
  virtual void who_am_i_really() { std::cout << "Really Erods" << std::endl; }
};

struct Fermat : public Erdos {
  void who_am_i() { std::cout << "Fermat" << std::endl; }
  virtual void who_am_i_really() { std::cout << "Fermat" << std::endl; }
};

// More complicated polymorphism.
// Vtable is normally  stored in const region.
//
// On heap            On const region
// ========            =======
// Complex2         +>  Vtable
// ========         |  =======
// Vtable: 8 -> ... +   dtor* --------> ~Complex2() {...}
// --------            -------
//  real: 8            double(*) -----> double Complex2::abs() {...}
// --------            -------
//  img : 8
// --------
// Vtable are set by the constructor.
struct Complex2 {
  virtual ~Complex2() = default;
  virtual double abs() { return std::hypot(real, img); }
  double real;
  double img;
};

struct Derived2 : public Complex2 {
  virtual ~Derived2() = default;
  virtual double abs() { return std::hypot(std::hypot(real, img), angle); }
  double angle;
};

// Notice here, if you create A fermat1 it will prints
// "Really Erdos."
// Because after the Constructor set the vtable, the half
// constructed Fermat1 call who_am_i_really, which at this
// point points to Erdos1's vtable.
//
// When you are in a base class constructor, you are of tye
// type of that base class.
// Conclusion ! Never call virtual function in a constructor.
struct Erdos1 {
  Erdos1() { who_am_i_really(); }
  virtual void who_am_i_really() { std::cout << "Really Erods" << std::endl; }
};

struct Fermat1 : public Erdos {
  virtual void who_am_i_really() { std::cout << "Fermat" << std::endl; }
};

// YMMV, the layout not always the same.

int main(void) {
  std::cout << "Is Complex pod? " << std::is_pod<Complex>::value << std::endl;
  std::cout << "size " << sizeof(Complex) << std::endl;
  std::cout << "\n" << std::endl;

  std::cout << "Is Derived pod? " << std::is_pod<Derived>::value << std::endl;
  std::cout << "size " << sizeof(Derived) << std::endl;
  std::cout << "\n" << std::endl;

  std::cout << "Is LayotOfDerived pod? " << std::is_pod<LayotOfDerived>::value
            << std::endl;
  std::cout << "size " << sizeof(LayotOfDerived) << std::endl;
  std::cout << "\n" << std::endl;

  std::cout << "Is Complex1 pod? " << std::is_pod<Complex1>::value << std::endl;
  std::cout << "size " << sizeof(Complex1) << std::endl;
  std::cout << "\n" << std::endl;

  // polymorphism
  // both of them have a virtual table with a point to one method.
  // one pointer, 8 bytes.
  std::cout << "Is Erdos pod? " << std::is_pod<Erdos>::value << std::endl;
  std::cout << "size " << sizeof(Erdos) << std::endl;
  std::cout << "\n" << std::endl;

  std::cout << "Is Fermat pod? " << std::is_pod<Fermat>::value << std::endl;
  std::cout << "size " << sizeof(Fermat) << std::endl;
  std::cout << "\n" << std::endl;

  return 0;
}
