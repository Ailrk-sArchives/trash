#include <iostream>

// for some api, we can't access the underlying opqaue data, to
// manipulate certain data we need to reinterpret our struct to fit with the
// api.

typedef struct Opaque_ *VendorGlobalUserData;
void VendorSetUserData(VendorGlobalUserData);
VendorGlobalUserData VendorGetUserData();

struct MyUserData {
  int m;
  MyUserData() : m(42) {}
};

int main(void) {

  // set global data
  MyUserData d;
  VendorSetUserData(reinterpret_cast<VendorGlobalUserData>(&d));

  // get global data.
  VendorGlobalUserData d1 = VendorGetUserData();
  MyUserData *p = nullptr;
  p = reinterpret_cast<MyUserData *>(d1);

  if (p) {
    std::cout << p->m << std::endl;
  }
  return 0;
}
