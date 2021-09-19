#include "regex.c"

char buffer[2048];
int i = 0;

void push_buffer(char n, char *buffer, int *i) {
  if (n != '^') {
    buffer[*i] = n;
    (*i)++;
  }
}

void push_buffer_cb(node *n) { push_buffer(n->s, buffer, &i); }

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

void print_table() {
  char n;
  // readable ascii start from 33 to 127
  printf("    ");
  for (int i = 0; i < 10; ++i) {
    printf("%d ", i);
  }
  printf("\n");
  // for (int i = 0; i < MAX_STATE_NUM; ++i) {
  for (int i = 97; i < MAX_STATE_NUM - 27; ++i) {
    printf("%c | ", i);
    // for testing we won't use more than 10 states.
    // for (int j = 0; j < MAX_SYM_NUM; ++j) {
    for (int j = 0; j < 10; ++j) {
      n = '0' + table[i][j];
      printf("%c ", n);
    }
    printf("\n");
  }
}

void print_fnode(struct fnode *fn) {
  printf("final_states: ");
  struct fnode *p = final_states;
  while (p) {
    printf("%d ", p->state);
    p = p->next;
  }
  printf("\n");
}

void test1() {
  code = "ab*(a|c)";
  node *n = parse();

  PRINT_NODE(":: ", n);
  preorder_traversal(n, NULL, push_buffer_cb);
  printf(buffer);
  printf("\n");

  free_node(n);
  regex_clear();
}

void test2() {
  state = 0;
  code = "ab*(a|c)";
  stack = (char *)malloc(STACK_SZ);
  sp = stack;
  node *n = parse();

  preorder_traversal(n, mark_node, build_from_node);
  print_table();
  print_fnode(final_states);
  regex_clear();
}

#define MATCH_REGEX(pat, str)                                                  \
  {                                                                            \
    int m = regex(pat, str);                                                   \
    printf("%10s, %20s result: %d\n", pat, str, m);                                   \
  }

void test3() {

  MATCH_REGEX("ab*(a|c)", "aba");
  MATCH_REGEX("ab*(a|c)", "abbbbbba");
  MATCH_REGEX("ab*(a|c)", "abbbbc");
  MATCH_REGEX("ab*(a|c)", "abbbbd");

  MATCH_REGEX("ab(a*c|cd)", "abaac");
  MATCH_REGEX("ab(a*c|cd)", "abaacd");
}

int main(void) {
  /* test1(); */
  /* test2(); */
  test3();
  return 0;
}
