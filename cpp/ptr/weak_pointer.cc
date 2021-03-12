#include <iostream>
#include <memory>

// weak_ptr is used with the shared ptr. It doesn't increase the
// reference count, but it will still allow you to access the underlying
// resource.

std::weak_ptr<int> gw;

void observe() {
  std::cout << "use_count == " << gw.use_count() << std::endl;
  // has to be copied into a shared ptr to use.
  // this ensure while using the weak ref the underlying resource still
  // has at least one ref count and wont get free during usage.
  if (auto spt = gw.lock()) {
    std::cout << *spt << std::endl;
  }

  else {
    std::cout << "gw is expired" << std::endl;
  }
}

static_assert(sizeof(std::shared_ptr<int>) == 16);
static_assert(sizeof(std::weak_ptr<int>) == 16);

int main(void) {
  {
    auto sp = std::make_shared<int>(52);

    // copy to a weak ref.
    gw = sp;
    observe();
  }
  observe();

  return 0;
}
