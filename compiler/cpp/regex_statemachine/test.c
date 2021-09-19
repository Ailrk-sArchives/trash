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

void print_table() {
  char n;
  // readable ascii start from 33 to 127
  printf("    ");
  for (int i = 0; i < 10; ++i) {
    printf("%d ", i);
  }
  printf("\n");
  // for (int i = 0; i < MAX_STATE_NUM; ++i) {
  for (int i = 97; i < MAX_STATE_NUM - 5; ++i) {
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

void test1() {
  code = "ab(bb*|ac)";
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
  code = "ab(bb*|ac)";
  stack = (char *)malloc(STACK_SZ);
  sp = stack;
  node *n = parse();

  preorder_traversal(n, mark_node, build_from_node);
  print_table();

  regex_clear();
}

int main(void) {

  // test1();
  test2();
  return 0;
}
