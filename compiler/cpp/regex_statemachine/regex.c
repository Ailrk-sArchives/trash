#include "regex.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STATE_NUM 128
#define MAX_SYM_NUM 128
#define STACK_SZ 128

char *code; // the input regex
char sym;   // current symbol, for parsing only.

char *stack; // stack for building the table
char *sp;    // stack pointer.
int state;   // current state.

// This regualr expression library supports matching 128 long
// ascii string.
//
// A regular expression engine is essentially a regular expression to
// state machine compiler. Given a langauge ∑ and a set of states Q,
// we have transition function δ: ∑ × Q → Q. That is, the state machine
// is a 2d table indexed by the current state and current symbol.
//
// nd Array is actually curried functions, fully applied nd array gives
// us the final mapping. In this case is the next state the machine goes
// to.
//
// So another view of regex engine is that we're building the arity 2
// mapping functions before hand so we can use it afterwards.
char table[MAX_SYM_NUM][MAX_STATE_NUM];
char final_state[MAX_STATE_NUM];

// clear regex state.
void regex_clear() {
  code = NULL;
  state = 0;
  sym = 0;
  stack = NULL;
  sp = NULL;
  memset(table, 0, MAX_SYM_NUM * MAX_STATE_NUM * sizeof(char));
  memset(final_state, 0, MAX_STATE_NUM * sizeof(char));
}

node *new_node(char s, node *left, node *right) {
  node *n = (node *)malloc(sizeof(node));
  n->s = s;
  n->left = left;
  n->right = right;
  return n;
}

void print_node(node *n) {
  if (n == NULL)
    return;
  printf("<%c ", n->s);
  if (n->left)
    print_node(n->left);
  else
    printf(" _ ");
  if (n->right)
    print_node(n->right);
  else
    printf(" _ ");
  printf("> ");
}

// pass preprocess and cb to customize action to perform on preorder
// traversal.
void preorder_traversal(node *n, void (*preprocess)(node *),
                        void (*cb)(node *)) {
  if (preprocess)
    preprocess(n);
  if (n->left)
    preorder_traversal(n->left, preprocess, cb);
  if (cb)
    cb(n);
  if (n->right)
    preorder_traversal(n->right, preprocess, cb);
}

#define PRINT_NODE(msg, n)                                                     \
  printf(msg "\n");                                                            \
  print_node(n);                                                               \
  printf("\n");

void free_node(node *n) {
  if (n->left)
    free_node(n->left);
  if (n->right)
    free_node(n->right);
  free(n);
}

void error(char const msg[]) {
  fprintf(stderr, msg);
  fprintf(stderr, "\n");
}

void nextsym() {
  static int pos = 0;
  if (code[pos] != '\0')
    sym = code[pos++];
  else
    sym = '\0';
}

int accept(char s) {
  if (sym == s) {
    nextsym();
    return 1;
  } else
    return 0;
}

// accept an char in range a-z, return the accepted char.
char accept_alpha() {
  if (sym >= 'a' && sym <= 'z') {
    char n = sym;
    nextsym();
    return n;
  } else
    return 0;
}

int expect(char s) {
  if (accept(s))
    return 1;
  error("expect: unexpected token");
  return 0;
}

// EBNF for regex:
//   factor = char | "(" expression ")"
//   term = factor ["*"]
//   term1 = term { term }
//   expression = term1 { "|" term1 }
//
// note: regular expression has two operations, . and |
// . is a non comutative monoid, and | is a group. let empty string be
// the identity then it forms a non conmutative ring.
//
// Here we omit . operator, so there is no need to define . as a node
// in the syntax. However we still need to convert it into an ast node.

// A recursive decent parser for regex.
node *expression();

node *factor() {
  if (accept('(')) {
    node *e;
    e = expression();
    expect(')');
    return e;
  } else {
    char s;
    s = accept_alpha();
    if (s > 1) {
      node *p = new_node(s, NULL, NULL);
      return p;
    } else {
      printf("factor error %c\n", sym);
      error("factor: syntax error");
      nextsym();
    }
    return NULL;
  }
}

node *term() {
  node *f;
  f = factor();
  if (accept('*')) {
    node *klenee_star = new_node('*', f, NULL);
    return klenee_star;
  }
  return f;
}

node *term1() {
  node *t1, *t2, *o;
  o = term();
  while (isalpha((int)sym) || sym == '(') {
    t1 = o;
    o = new_node('.', NULL, NULL);
    t2 = term();
    if (t2) {
      o->left = t1;
      o->right = t2;
      t1 = o;
    } else {
      error("term1: term { term }");
      nextsym();
    }
  }
  return o;
}

node *expression() {
  node *t1, *t2, *o;
  o = term1();
  while (accept('|')) {
    t1 = o;
    o = new_node('|', NULL, NULL);
    t2 = term1();
    if (t2) {
      o->left = t1;
      o->right = t2;
      t1 = o;
    } else {
      error("expression: error for term1 { [\"|\"] term1 } ");
      nextsym();
    }
  }
  return o;
}

node *parse() {
  nextsym();
  return expression();
}

// callbacks for preorder traversal.

void mark_node(node *n) {
  if (n->s == '|') {
    // when hit a |, push | and current state.
    // the top of the stack looks like
    // ( -- saved_state | )
    // next time we hit | we know | is unfinished, so we can continue
    // to fill the table for saved_state.
    *sp++ = (char)state; // record the current state when hit a |
    *sp++ = '|';
    printf("fisrt | : %d\n", state);
  }

  if (n->s == '*') {
    // when hit a *, we first save the current state. but to repeat
    // the next child we also need to know the next symbol.

    *sp++ = (char)state;
    *sp++ = '*';
    printf("first * : %d\n", state);
  }
}

// A state machine table is a transtion function indexed by symbol and
// state. Thus transitin : Sym -> State
//   0 1 2 3 4
// a 1 0 3 0 0
// b 0 2 2 0 0  for regex ab*(a|c)
// c 0 0 4 0 0
//
//                .
//              /   \
//          0 .       |
//           / \     / \
//          a   *   a   c
//          1   |   3   4
//              b
//              2
//
//
void build_from_node(node *n) {
  char sym;
  static int sig = 0;

  if (n->s == '|') { // hit | the second time.
    // note: / has the shape
    //    |
    //   / \
    //  A   B
    // let state before a be x. then state after a is x + 1, after b is
    // x + 2.
    // we save the state x on the stack, and when the second time we
    // visit |, where we are about to visist b, we can signal b the
    // state s.
    --sp;
    sig = *--sp;

  } else if (n->s == '*') {
    // note: * in the ast has the shape
    //      *
    //     / \
    //    n   _
    // let state at * = x. after n state = x + 1.
    // the next time hit * we want to set table[n][x + 1] back to
    // x, so we can repeat the process.
    --sp;
    sig = *--sp;
  } else if (isalnum(n->s)) {
    sym = n->s;
    if (sig) {
      printf("sig %d\n", sig);
      table[sym][sig] = state++;
      sig = 0;
    } else {
      int prev_state = state++;
      table[sym][prev_state] = state;
    }
  }
}

int regex(char pat[], char str[]) {
  // a langauge with 256 state and 256 symbols maximum.
  // state is indexed by chars.
  node *expr;

  regex_clear();

  {
    state = 0;
    code = pat;
    stack = (char *)malloc(STACK_SZ);
    sp = stack;
    expr = parse();
    preorder_traversal(expr, mark_node, build_from_node);
    free_node(expr);
  }

  {
    state = 0;
    for (char *p = str; *p != '\0'; ++p) {
      state = table[*p][state];
    }

    for (int i = 0; i < MAX_STATE_NUM && final_state[i] == state; ++i) {
      if (final_state[i] == '\0')
        return 0;
    }
  }
  return 1;
}
