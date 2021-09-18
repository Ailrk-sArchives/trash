#include "regex.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

static char *code;

char sym;

typedef struct node_ {
  char s;
  struct node_ *left;
  struct node_ *right;
} node;

node *new_node(char s, node *left, node *rigth) {
  node *n = (node *)malloc(sizeof(node));
  n->s = s;
  n->left = left;
  n->right = rigth;
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
  else {
    sym = '\0';
  }
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
    ;
  error("expect: unexpected token");
  return 0;
}

// factor = char | "(" expression ")"
// term = factor ["*"]
// term1 = term { term }
// expression = term1 { "|" term1 }

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
  }
  return NULL;
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
    o = new_node('^', NULL, NULL);
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

void mark_node(node *n, char *stack, int *state) {
  switch (n->s) {
  case '|':
    *stack++ = *state;
    *stack++ = '|';
    break;
  case '^':
    *stack++ = *state;
    *stack++ = '^';
    break;
  }
}

void build_from_node(node *n, char *stack, int *state) {
  int current_state = *state;
  (*state)++;
  switch (n->s) {
  case '|':
    // TODO jump back
    if (*stack == '|') {
      stack--;
      int last_state = *state--;
    }
    break;
  case '^':
    if (*stack == '^') {
      stack--;
      stack--;
    }
    break;
  default:
    break;
  }
}

#define MAX_STATE_NUM 128
#define MAX_SYM_NUM 128
int regex(char pat[], char str[]) {
  // a langauge with 256 state and 256 symbols maximum.
  // state is indexed by chars.
  static int table[MAX_STATE_NUM][MAX_SYM_NUM];
  static int final_state[MAX_STATE_NUM];
  int state;

  code = pat;
  {
    node *expr = parse();
    free_node(expr);
  }

  printf("new new\n");

  state = 0;
  for (char *p = str; *p != '\0'; ++p) {
    state = table[state][*p];
  }

  // searching really just boils down to a table querying.
  for (int i = 0; i < MAX_STATE_NUM && final_state[i] == state; ++i) {
    if (final_state[i] == '\0')
      return 0;
  }
  return 1;
}

#define Test
#ifdef Test

char buffer[2048];
int i = 0;

void push_buffer(char n, char *buffer, int *i) {
  if (n != '^') {
    buffer[*i] = n;
    (*i)++;
  }
}

void push_buffer_cb(node *n) { push_buffer(n->s, buffer, &i); }

void test() {
  {
    char *pat = "ab(bb*|ac)";

    code = pat;
    node *n = parse();

    PRINT_NODE(": ", n);
    preorder_traversal(n, NULL, push_buffer_cb);
    printf(buffer);
    printf("\n");
  }
}

#endif

int main(void) {

  test();
  return 0;
}
