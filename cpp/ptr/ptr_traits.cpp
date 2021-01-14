#include <iostream>
#include <memory>

template <typename Ptr> struct BlockList {

  struct block;
  using block_ptr_t = typename std::pointer_traits<Ptr>::template rebind<block>;

  struct block {
    size_t size;
    block_ptr_t next_block;
  };

  block_ptr_t free_blocks;
};

int main(void) {
  BlockList<int *> bl1;
  BlockList<std::shared_ptr<char>> bl2;
  return 0;
}
