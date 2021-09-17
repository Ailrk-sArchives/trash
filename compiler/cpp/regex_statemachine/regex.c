#include "regex.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

static char *code;
char sym;
struct node {
  char s;
  node *left;
  node *right;
};

/* static int next_state(int cur_state, int cur_symbol) { */
/*   return table[cur_state][cur_state]; */
/* } */

// it's interesting that the syntax for regular expression itself is
// context free, so we need a proper parser to parse it.

// ab(bb*|cb)
// the goal is to convert the graph into a grid.
// final state = 3, 5
//   0 1 2 3 4 5
// a 1
// b   2 3 3 5
// c     4

void free_node(node *n) {
  if (n->left) {
    free_node(n->left);
  }
  if (n->right) {
    free_node(n->right);
  }
  free(n);
}

void nextsym() {
  static int pos = 0;
  if (code[pos] != '\0') {
    sym = code[pos];
    pos++;
  }
}

void error(char const msg[]) { fprintf(stderr, msg); }
int accept(char s) {
  // only takes a-z, |, *., \\, (, )
  // '@ is the placeholder for all a-z'
  if (sym == s || (s == '@' && isalpha(sym))) {
    nextsym();
    return 1;
  } else
    return 0;
}

int expect(char s) {
  if (accept(s)) {
    ;
  }
  error("expect: unexpected token");
  return 0;
}

// binop = "|" | ""
// unary = "*"
// factor = char | "(" expression ")"
// term = factor ["*"]
// expression = term { ["|"] term }

node *expression();

node *factor() {
  if (accept('@')) {
    node *p = (node *)malloc(sizeof(node));
    p->s = sym;
    return p;
  } else if (accept('(')) {
    node *e = expression();
    expect(')');
    return e;
  } else {
    error("factor: syntax error");
    nextsym();
  }
  return NULL;
}

node *term() {
  node *f = factor();
  if (sym == '*') {
    nextsym();
    node *klenee_star = (node *)malloc(sizeof(node));
    klenee_star->left = f;
    return klenee_star;
  }
  return f;
}

node *expression() {
  node *t1 = term();
  if (sym != '|' || !isalpha(sym))
    return t1;

  node *o;
  while (sym == '|' || isalpha(sym)) {
    o = (node *)malloc(sizeof(node));
    o->s = sym == '|' ? '|' : '@';
    nextsym();
    node *t2 = term();
    o->left = t1;
    o->right = t2;
  }
  return o;
}

static void build_from_node(node *n) {
  switch (n->s) {
  case '|':
    break;
  case '@':
    break;
  default:
    break;
  }
}

int regex(char pat[], char str[]) {
  // a langauge with 256 state and 256 symbols maximum.
  // state is indexed by chars.
  static int table[128][128];
  static int final_state[128];

  code = pat;
  {
    node *expr = expression();
    build_from_node(expr);
    free_node(expr);
  }

  int state = 0;
  for (char *p = str; *p != '\0'; ++p) {
    state = table[state][*p];
  }

  // searching really just boils down to a table querying.
  for (int i = 0; i < 128 && final_state[i] == state; ++i) {
    if (final_state[i] == '\0')
      return false;
  }
  return true;
}
