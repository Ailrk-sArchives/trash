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

// insert instruction into the current buffer
void insert(Instrubuf *buf, int size, uint64_t ins) {}

// insert immediate value into the current buffer.
void immediate(Instrubuf *buf, int size, const void *value) {}

} // namespace instrbuf

// ideally we might want something like runtime assembler, so we can assemble
// assembly code from string and dump it into the mmap memory direclty.
//
// Or even better, a parser that parses a simple ir which get compiled to
// assembly and run in the memory diretly.
//
int main(void) {
  instrbuf::Instrubuf *buf{};
  instrbuf::finalize(buf);

  // cast the memory into a function pointer so we can call it directly..
  auto recurrence = reinterpret_cast<auto (*)(long)->long>(buf->code);

  int sol[10];
  for (int n = 0; n < 10; n++) {
    sol[n + 1] = recurrence(sol[n]);
  }
  return 0;
}
