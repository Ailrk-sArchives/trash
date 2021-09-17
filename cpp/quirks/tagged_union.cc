#include <iostream>
#include <string>
#include <vector>

// tagged union to represent algebraic data type.

enum TUENUM { TU_STRING, TU_INT, TU_FLOAT };

struct TU {
  int type;

  union U {

    struct { int type; int val; } i;
    struct { int type; float val; } f;
    // note for struct we have a default destructor.
    struct s_type_ { int type; std::string val; } s;

    U(int i) : i{TU_INT, i} {}
    U(float f) : f{TU_FLOAT, f} {}
    U(std::string s) : s{TU_STRING, std::move(s)} {}
    U(U const &other) {
      switch (other.i.type) {
      case TU_INT:
        ::new (&i) auto(other.i);
        break;
      case TU_FLOAT:
        ::new (&f) auto(other.f);
        break;
      case TU_STRING:
        ::new (&s) auto(other.s);
        break;
      }
    }

    ~U() {
      if (TU_STRING == s.type) {
        s.~s_type_();
      }
    }
  } u;

  int get_type() { return u.i.type; }

  bool match(TUENUM n) { return u.i.type == n; }

  TU(int i) : u(i) {}
  TU(float f) : u(f) {}
  TU(std::string s) : u(std::move(s)) {}
  TU(TU const &) = default;
  ~TU() = default;
};

int main(void) {
  auto a = TU("sdf");
  auto b = TU(1);

  if (a.match(TU_STRING)) {
    std::cout << a.u.s.val << std::endl;
  }

  if (b.match(TU_INT)) {
    std::cout << b.u.i.val << std::endl;
  }
  return 0;
}
