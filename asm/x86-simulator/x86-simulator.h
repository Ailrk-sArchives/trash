#ifndef X86_SIMULATOR_
#define X86_SIMULATOR_ value

// simulate c calling convention
// It's just a rough simulation.
// assume it's a 64 bit machine

#include <cstring>
#include <functional>
#include <inttypes.h>
#include <iostream>

// we have three segemetations
// ------------
// Stack 0          grow down
// ------------
// Heap 128 * 7 - 1 grow up
// ------------
// Data 128 * 7

#define STACK_BASE 256 * 8
#define HEAP_BASE 256 * 4
#define DATA_BASE 256
#define TEXT_BASE 0

// status register
struct CCR {
  int EM1 : 1;
  int EM2 : 1;
  int EM3 : 1;

  int X : 1; // extended
  // note:
  // There is no sign in two's complement system
  // what so ever. V and C have different meaning
  // depends on how you interpret your number.

  int N : 1; // sign flag
  int Z : 1; // zero
  int V : 1; // overflow
  int C;     // carry bit, we don't touch this
};

struct Register {
  // no ip reg.
  size_t rip;
  uint64_t rsp;
  uint64_t rbp;
  uint64_t rax;
  uint64_t rbx;
  uint64_t rcx;
  uint64_t rdx;
  uint64_t rsi;
  uint64_t rdi;
  CCR ccr;
};

static Register regfile{.rip = 0,
                        .rsp = STACK_BASE,
                        .rbp = STACK_BASE,
                        .rax = 0,
                        .rbx = 0,
                        .rcx = 0,
                        .rdx = 0,
                        .rsi = 0,
                        .rdi = 0,
                        .ccr = CCR{}};
struct Address {
  uint64_t address;
};

// for simplicity,
// we have in total 256 addresses
static char memory[256 * 8];

#define r regfile
#define m memory
// instructions
// we use intel convention, so all instructions has
// their destination on the left.

void move(uint64_t *reg1, uint64_t *reg2) { *reg1 = *reg2; }
void move(uint64_t *reg1, Address addr) { *reg1 = memory[addr.address]; }
void move(uint64_t addr, uint64_t *reg2) { memory[addr] = *reg2; }
void move(uint64_t *reg1, const uint64_t val) { *reg1 = val; }
void move(uint64_t addr, const uint64_t val) { memory[addr] = val; }

void swap_endianess(uint64_t val) {
  unsigned char *bytes = reinterpret_cast<unsigned char *>(&val);
  for (int i = 0; i < sizeof(val); i += 2) {
    unsigned char temp = std::move(bytes[i]);
    bytes[i] = std::move(bytes[sizeof(val) - i]);
    bytes[sizeof(val) - i] = temp;
  }
}

// push
// rsp-4 <- val
void push(uint64_t *reg) {
  regfile.rsp -= 8;
  swap_endianess(*reg);
  memcpy(memory + regfile.rsp, reg, sizeof(uint64_t));
}
void push(Address addr) {
  regfile.rsp -= 8;
  swap_endianess(*(memory + addr.address));
  memcpy(memory + regfile.rsp, (memory + addr.address), sizeof(uint64_t));
}
void push(const uint64_t val) {
  swap_endianess(val);
  regfile.rsp -= 8;
  memcpy(memory + regfile.rsp, &val, sizeof(uint64_t));
}

// pop
// rsp+4 -> val
void pop(uint64_t *reg) {
  auto old_rsp = regfile.rsp;
  regfile.rsp += 8;
  memcpy(reg, memory + old_rsp, sizeof(uint64_t));
}

void pop(Address addr) {
  auto old_rsp = regfile.rsp;
  regfile.rsp += 8;
  memcpy(memory + addr.address, memory + old_rsp, sizeof(uint64_t));
}

// lea
// only load the effective address
// reg <- mem
void lea(uint64_t *reg, Address addr) { *reg = addr.address; }

#define SET_CCR_Z(from)                                                        \
  if (from == 0) {                                                             \
    regfile.ccr.Z = 1;                                                         \
  } else {                                                                     \
    regfile.ccr.Z = 0;                                                         \
  }

// reg reg
static std::function<void(uint64_t *, uint64_t *)>
mk_binop_reg_reg(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](uint64_t *reg1, uint64_t *reg2) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    *reg1 = op(*reg1, *reg2);
    SET_CCR_Z(*reg1);
  };
}

// reg addr
static std::function<void(uint64_t *, Address)>
mk_binop_reg_addr(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](uint64_t *reg, Address addr) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    *reg = op(*reg, memory[addr.address]);
    SET_CCR_Z(*reg);
  };
}

// addr reg
static std::function<void(Address, uint64_t *)>
mk_binop_addr_reg(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](Address addr, uint64_t *reg) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    memory[addr.address] = op(memory[addr.address], *reg);
    SET_CCR_Z(memory[addr.address]);
  };
}

// reg const
static std::function<void(uint64_t *, const uint64_t)>
mk_binop_reg_const(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](uint64_t *reg, const uint64_t val) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    *reg = op(*reg, val);
    SET_CCR_Z(*reg);
  };
}

// mem const
static std::function<void(Address, const uint64_t)>
mk_binop_addr_const(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](Address addr, const uint64_t val) {
    memory[addr.address] = op(memory[addr.address], val);
    SET_CCR_Z(memory[addr.address]);
  };
}

// when we call this functions our ccr is always clear.
static auto add_ = [](uint64_t a, uint64_t b) {
  auto result = a + b;
  if (a > 0 && b > 0 && result < 0) {
    regfile.ccr.V = 1;
  }
  regfile.ccr.N = (result < 0);
  return result;
};

static auto sub_ = [](uint64_t a, uint64_t b) {
  auto result = a - b;
  if (a < 0 && b < 0 && result > 0) {
    regfile.ccr.V = 1;
  }
  if (a > b) {
    regfile.ccr.C = 1;
  }
  return result;
};

static auto and_ = [](uint64_t a, uint64_t b) { return a & b; };
static auto or_ = [](uint64_t a, uint64_t b) { return a | b; };
static auto xor_ = [](uint64_t a, uint64_t b) { return a ^ b; };
static auto nor_ = [](uint64_t a, uint64_t _) { return !a; };
static auto shl_ = [](uint64_t a, uint64_t b) { return a << b; };
static auto shr_ = [](uint64_t a, uint64_t b) { return a >> b; };

// cmp perform sub but only use the side effect.
auto cmp_ = [](uint64_t a, uint64_t b) {
  sub_(a, b);
  return a; // do nothing
};

// add
auto add_reg_reg = mk_binop_reg_reg(add_);
auto add_reg_addr = mk_binop_reg_addr(add_);
auto add_addr_reg = mk_binop_addr_reg(add_);
auto add_reg_const = mk_binop_reg_const(add_);
auto add_addr_const = mk_binop_addr_const(add_);

// sub
auto sub_reg_reg = mk_binop_reg_reg(sub_);
auto sub_reg_addr = mk_binop_reg_addr(sub_);
auto sub_addr_reg = mk_binop_addr_reg(sub_);
auto sub_reg_const = mk_binop_reg_const(sub_);
auto sub_addr_const = mk_binop_addr_const(sub_);

// and
auto and_reg_reg = mk_binop_reg_reg(and_);
auto and_reg_addr = mk_binop_reg_addr(and_);
auto and_addr_reg = mk_binop_addr_reg(and_);
auto and_reg_const = mk_binop_reg_const(and_);
auto and_addr_const = mk_binop_addr_const(and_);

// or
auto or_reg_reg = mk_binop_reg_reg(or_);
auto or_reg_addr = mk_binop_reg_addr(or_);
auto or_addr_reg = mk_binop_addr_reg(or_);
auto or_reg_const = mk_binop_reg_const(or_);
auto or_addr_const = mk_binop_addr_const(or_);

// xor
auto xor_reg_reg = mk_binop_reg_reg(xor_);
auto xor_reg_addr = mk_binop_reg_addr(xor_);
auto xor_addr_reg = mk_binop_addr_reg(xor_);
auto xor_reg_const = mk_binop_reg_const(xor_);
auto xor_addr_const = mk_binop_addr_const(xor_);

// cmp
auto cmp_reg_reg = mk_binop_reg_reg(cmp_);
auto cmp_reg_addr = mk_binop_reg_addr(cmp_);
auto cmp_reg_const = mk_binop_reg_const(cmp_);
auto cmp_addr_reg = mk_binop_addr_reg(cmp_);

auto shl_reg_const = mk_binop_addr_reg(shl_);
auto shl_addr_const = mk_binop_addr_const(shl_);

auto shr_reg_const = mk_binop_addr_reg(shr_);
auto shr_addr_const = mk_binop_addr_const(shr_);

void inc(uint64_t *reg) { (*reg)++; }
void inc(Address addr) { memory[addr.address]++; }
void dec(uint64_t *reg) { (*reg)--; }
void dec(Address addr) { memory[addr.address]--; }
void not_(uint64_t *reg) { *reg = ~(*reg); }
void not_(Address addr) { memory[addr.address] = ~memory[addr.address]; }

#define jmp(label) goto label
#define je(label)                                                              \
  if (regfile.ccr.Z != 0) {                                                    \
    goto label;                                                                \
  }

#define jne(label)                                                             \
  if (regfile.ccr.Z == 0) {                                                    \
    goto label;                                                                \
  }

#define jz(label)                                                              \
  if (regfile.ccr.Z == 0) {                                                    \
    goto label;                                                                \
  }

#define jg(label)                                                              \
  if (regfile.ccr.C == 1 && regfile.ccr.N != 0) {                              \
    goto label;                                                                \
  }

#define jge(label)                                                             \
  if (regfile.ccr.C == 1) {                                                    \
    goto label;                                                                \
  }

#define jl(label)                                                              \
  if (regfile.ccr.C != 1 && regfile.ccr.N != 0) {                              \
    goto label;                                                                \
  }

#define jle(label)                                                             \
  if (regfile.ccr.C != 1) {                                                    \
    goto label;                                                                \
  }

#define CONCAT_(x, y) x##y
#define CONCAT(x, y) CONCAT_(x, y)

// push current ip
//
// Note && to get the address of label is
// a gnu extension

#define call_(label, counter)                                                  \
  push((const uint64_t) && CONCAT(label, counter));                            \
  goto label;                                                                  \
  CONCAT(label, counter) :

#define call(label) call_(label, __COUNTER__)

#define ret                                                                    \
  pop(&regfile.rip);                                                           \
  goto *regfile.rip;

// you can use the c calling convention
// the convention comprises two parts for caller and callee rrspectively.
//
#endif /* ifndef X86_SIMULATOR_ */
