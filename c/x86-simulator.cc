// simulate c calling convention
// It's just a rough simulation.
// assume it's a 64 bit machine

#include <cstring>
#include <functional>
#include <inttypes.h>
#include <iostream>
#include <tuple>

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
  uint64_t esp;
  uint64_t ebp;
  uint64_t eax;
  uint64_t ebx;
  uint64_t ecx;
  uint64_t edx;
  uint64_t esi;
  uint64_t edi;
  CCR ccr;
};

static Register regfile;
typedef struct {
  uint64_t address;
} Address;

// for simplicity,
// we have in total 256 addresses
static char memory[256 * 4];

// we have three segemetations
// ------------
// Stack 0          grow down
// ------------
// Heap 128 * 7 - 1 grow up
// ------------
// Data 128 * 7

#define STACK_BASE 256 * 4
#define HEAP_BASE 256 * 2
#define DATA_BASE 256
#define TEXT_BASE 0

// instructions
// we use intel convention, so all instructions has
// their destination on the left.

void I_move(uint64_t *reg1, uint64_t *reg2) { *reg1 = *reg2; }
void I_move(uint64_t *reg1, Address addr) { *reg1 = memory[addr.address]; }
void I_move(uint64_t addr, uint64_t *reg2) { memory[addr] = *reg2; }
void I_move(uint64_t *reg1, const uint64_t val) { *reg1 = val; }
void I_move(uint64_t addr, const uint64_t val) { memory[addr] = val; }

// push
// esp-4 <- val
void I_push(uint64_t *reg) {
  auto old_esp = regfile.esp;
  regfile.esp -= 4;
  memcpy(memory + old_esp, reg, sizeof(*reg));
}
void I_push(Address addr) {
  auto old_esp = regfile.esp;
  regfile.esp -= 4;
  memcpy(memory + old_esp, (memory + addr.address), sizeof(memory[0] * 4));
}
void I_push(const uint64_t val) {
  auto old_esp = regfile.esp;
  regfile.esi -= 4;
  memcpy(memory + old_esp, &val, sizeof(memory[0] * 4));
}

// pop
// esp+4 -> val
void I_pop(uint64_t *reg) {
  regfile.esp += 4;
  memcpy(reg, memory + regfile.esp, sizeof(memory[0] * 4));
}

void I_pop(Address addr) {
  regfile.esp += 4;
  memcpy(memory + addr.address, memory + regfile.esp, sizeof(memory[0] * 4));
}

// lea
// only load the effective address
// reg <- mem
void I_lea(uint64_t *reg, Address addr) { *reg = addr.address; }

#define SET_CCR_Z(from)                                                        \
  if (from == 0) {                                                             \
    regfile.ccr.Z = 1;                                                         \
  } else {                                                                     \
    regfile.ccr.Z = 0;                                                         \
  }

// reg reg
std::function<void(uint64_t *, uint64_t *)>
mk_binop_reg_reg(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](uint64_t *reg1, uint64_t *reg2) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    *reg1 = op(*reg1, *reg2);
    SET_CCR_Z(*reg1);
  };
}

// reg addr
std::function<void(uint64_t *, Address)>
mk_binop_reg_addr(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](uint64_t *reg, Address addr) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    *reg = op(*reg, memory[addr.address]);
    SET_CCR_Z(*reg);
  };
}

// addr reg
std::function<void(Address, uint64_t *)>
mk_binop_addr_reg(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](Address addr, uint64_t *reg) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    memory[addr.address] = op(memory[addr.address], *reg);
    SET_CCR_Z(memory[addr.address]);
  };
}

// reg const
std::function<void(uint64_t *, const uint64_t)>
mk_binop_reg_const(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](uint64_t *reg, const uint64_t val) {
    std::memset(&regfile.ccr, 0, sizeof(regfile.ccr));
    *reg = op(*reg, val);
    SET_CCR_Z(*reg);
  };
}

// mem const
std::function<void(Address, const uint64_t)>
mk_binop_addr_const(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return [=](Address addr, const uint64_t val) {
    memory[addr.address] = op(memory[addr.address], val);
    SET_CCR_Z(memory[addr.address]);
  };
}

auto mk_binops(std::function<uint64_t(uint64_t, uint64_t)> op) {
  return std::make_tuple(mk_binop_reg_reg(op), mk_binop_reg_addr(op),
                         mk_binop_addr_reg(op), mk_binop_addr_reg(op),
                         mk_binop_addr_const(op));
}

// when we call this functions our ccr is always clear.
auto add_ = [](uint64_t a, uint64_t b) {
  auto result = a + b;
  if (a > 0 && b > 0 && result < 0) {
    regfile.ccr.V = 1;
  }
  regfile.ccr.N = (result < 0);
  return result;
};

auto sub_ = [](uint64_t a, uint64_t b) {
  auto result = a - b;
  if (a < 0 && b < 0 && result > 0) {
    regfile.ccr.V = 1;
  }
  if (a > b) {
    regfile.ccr.C = 1;
  }
  return result;
};

auto and_ = [](uint64_t a, uint64_t b) { return a & b; };
auto or_ = [](uint64_t a, uint64_t b) { return a | b; };
auto xor_ = [](uint64_t a, uint64_t b) { return a ^ b; };
auto nor_ = [](uint64_t a, uint64_t _) { return !a; };
auto shl_ = [](uint64_t a, uint64_t b) { return a << b; };
auto shr_ = [](uint64_t a, uint64_t b) { return a >> b; };

// cmp perform sub but only use the side effect.
auto cmp_ = [](uint64_t a, uint64_t b) {
  sub_(a, b);
  return a; // do nothing
};

// binary operators
auto [I_add_reg_reg, I_add_reg_addr, I_add_addr_reg, I_add_reg_const,
      I_add_addr_const] = mk_binops(add_);

auto [I_sub_reg_reg, I_sub_reg_addr, I_sub_addr_reg, I_sub_reg_const,
      I_sub_addr_const] = mk_binops(sub_);

auto [I_and_reg_reg, I_and_reg_addr, I_and_addr_reg, I_and_reg_const,
      I_and_addr_const] = mk_binops(and_);

auto [I_or_reg_reg, I_or_reg_addr, I_or_addr_reg, I_or_reg_const,
      I_or_addr_const] = mk_binops(or_);

auto [I_xor_reg_reg, I_xor_reg_addr, I_xor_addr_reg, I_xor_reg_const,
      I_xor_addr_const] = mk_binops(xor_);

// cmp
auto I_cmp_reg_reg = mk_binop_reg_reg(cmp_);
auto I_cmp_reg_addr = mk_binop_reg_addr(cmp_);
auto I_cmp_reg_const = mk_binop_reg_const(cmp_);
auto I_cmp_addr_reg = mk_binop_addr_reg(cmp_);

auto I_shl_reg_const = mk_binop_addr_reg(shl_);
auto I_shl_addr_const = mk_binop_addr_const(shl_);

auto I_shr_reg_const = mk_binop_addr_reg(shr_);
auto I_shr_addr_const = mk_binop_addr_const(shr_);

void I_inc(uint64_t *reg) { (*reg)++; }
void I_inc(Address addr) { memory[addr.address]++; }
void I_dec(uint64_t *reg) { (*reg)--; }
void I_dec(Address addr) { memory[addr.address]--; }
void I_not(uint64_t *reg) { *reg = ~(*reg); }
void I_not(Address addr) { memory[addr.address] = ~memory[addr.address]; }

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

// push current ip
// Note && to get the address of label is
// a gnu extension
#define call(label)                                                            \
  call##label : I_push(&&call##label);                                         \
  goto label;

#define ret                                                                    \
  I_pop(&regfile.rip);                                                         \
  goto *regfile.rip;

int main(void) {

  Register r = regfile;
main_:

  I_push(&r.esi);
  I_push(&r.edi);

  I_move(&r.esi, 1);
  I_move(&r.edi, 1);

L1:

  I_push(&r.esi);
  I_push(&r.esi);

  return 0;
}
