#include <cstring>
#include <iostream>
#include <sys/mman.h>
#include <unistd.h>

// https://nullprogram.com/blog/2015/03/19/

// OS has page granularity protection mechanism. different parts of a process
// have different proection. e.g "read", "write", and "execute".

// If we want to jit compile anything, we need to ask for a executable piece
// of memory and put instruction there directly.

// posix sysconf(_SC_PAGESIZE) gets configuration info at runtime. It
// can be used to discover the paage size. Here we just assume page_size to
// be 4096 bytes.

namespace instrbuf {
#define PAGE_SIZE 4096

// a little trick to give a section of the array an accessor.
typedef struct {
  uint8_t code[PAGE_SIZE - sizeof(uint64_t)];
  uint64_t count;
} Instrubuf;

// to ask for a piece of mmaped memory we need to provide two pieces of configs
// 1. the protection bits of the page
// 2. the config of the map itself.
// MAP_ANONYMOUS means the memory is not backed by any files.
//
// Another thing:
//  linux has W^X security feature, a memory never be writable and executable
//  at the same time. This is important for security reasons, as you can't
//  inject arbitrary code at runtime.
//
//  However this is getting in our way, we do want to write a execute.
//  we need to adjust the memory protection levels at runtime. To get the
//  permission we need.
Instrubuf *create() {
  int prot = PROT_READ | PROT_WRITE;
  int flags = MAP_ANONYMOUS | MAP_PRIVATE;
  return static_cast<Instrubuf *>(mmap(NULL, PAGE_SIZE, prot, flags, -1, 0));
}

// change the memory protection at the runtime.

void finalize(Instrubuf *buf) {
  mprotect(buf, sizeof(*buf), PROT_READ | PROT_EXEC);
}

void ifree(Instrubuf *buf) { munmap(buf, PAGE_SIZE); }

// insert instruction into the current buffer directly
// number of bytes to insert depends on size we pass in.
//
// uint64_t is merely a type that long enough to contain instructions.
//
// what does (i * 8) & 0xff do?
//  0xff just takes the least significant byte.
//  as buf->code[n] = will only takes a byte.
//  for instruction `add rax rdi`
//  we load add, rax, rdi respectively, where each operator takes 1 byte.
//    PS: actually the order is add rdi rax, since intel syntax reverse the
//        order of the src and dest.
void insert(Instrubuf *buf, int size, uint64_t ins) {
  for (int i = size - 1; i >= 0; --i) {
    buf->code[buf->count++] = (ins >> (i * 8)) & 0xff;
  }
}

// insert immediate value into the current buffer.
void immediate(Instrubuf *buf, int size, const void *value) {
  memcpy(buf->code + buf->count, value, size);
  buf->count += size;
}

// so what would we insert into the memory as program? We can insert bunch of
// assembly code, and follow the normal c calling convention. In this way, we
// can cast the pointer to the memory as a function pointer, and call the
// memory direclty.
//
// for c calling convention, the return value will be in rax.

} // namespace instrbuf

// here we assume all operands are in rdi already. This simplify stuffs a bit.

void compile_jit_op(instrbuf::Instrubuf *buf, char op) {
  switch (op) {
  case '+':
    instrbuf::insert(buf, 3, 0x4801f8); // add rax rdi
    break;
  case '-':
    instrbuf::insert(buf, 3, 0x4829f8); // sub rax rdi
    break;
  case '*':
    instrbuf::insert(buf, 3, 0x480fafc7); // imul rax rdi
    break;
  case '/':
    instrbuf::insert(buf, 3, 0x4801f8); // xor rdx rdx
    instrbuf::insert(buf, 3, 0x48f7ff); // idiv rdi
    break;
  }
}

void compile_jit(instrbuf::Instrubuf *buf) {
  instrbuf::insert(buf, 3, 0x4889F8); // mov rax rdi

  int c;

  while ((c = fgetc(stdin) != '\n' && c != EOF)) {
    if (c == ' ')
      continue;
    char op = c;
    long operand;
    scanf("%ld", &operand);
    instrbuf::insert(buf, 2, 0x48bf);      // mov operand %rdi
    instrbuf::immediate(buf, 8, &operand); // still the same instruction
    compile_jit_op(buf, op);
  }

  instrbuf::insert(buf, 1, 0xC3); // ret
  instrbuf::finalize(buf);
}

// ideally we might want something like runtime assembler, so we can assemble
// assembly code from string and dump it into the mmap memory direclty.
//
// Or even better, a parser that parses a simple ir which get compiled to
// assembly and run in the memory diretly.
//
int main(void) {
  instrbuf::Instrubuf *buf{};

  // cast the memory into a function pointer so we can call it directly..

  long init;
  unsigned long term;
  scanf("%ld %lu", &init, &term);
  auto recurence = reinterpret_cast<auto (*)(long)->long>(buf->code);
  for (unsigned long i = 0, x = init; i <= term; i++, x = recurence(x))
    fprintf(stderr, "Term %lu: %ld\n", i, x);

  instrbuf::ifree(buf);
  return 0;
}
