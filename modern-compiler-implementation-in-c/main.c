#include "ast.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

int main(void) {

  Astm *stm = compound_stm(
      assign_stm("a", biop_expr(num_expr(10), Adiv, num_expr(2))),
      print_stm(exprlist_pair(num_expr(2), exprlist_last(num_expr(2)))));

  free_astm(stm);
  return 0;
}
