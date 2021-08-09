#include "ast.h"
#include "util.h"
#include <assert.h>
#include <stdlib.h>

// consstructor
Astm *compound_stm(Astm *s1, Astm *s2) {
  Astm *s = (Astm *)checked_malloc(sizeof(*s));
  s->kind = AcompoundStm;
  s->u.compound.stm1 = s1;
  s->u.compound.stm2 = s2;
  return s;
}

Astm *assign_stm(string id, Aexpr *expr) {
  Astm *s = (Astm *)checked_malloc(sizeof(*s));
  s->kind = AassignStm;
  s->u.assign.id = id;
  s->u.assign.expr = expr;
  return s;
}

Astm *print_stm(AexprList *exps) {
  Astm *s = (Astm *)checked_malloc(sizeof(*s));
  s->kind = AprintStm;
  s->u.print.exps = exps;
  return s;
}

Aexpr *eseq_expr(Astm *stm, Aexpr *expr) {
  Aexpr *s = (Aexpr *)checked_malloc(sizeof(*s));
  s->kind = Aeseqexpr;
  s->u.eseqexpr.stm = stm;
  s->u.eseqexpr.expr = expr;
  return s;
}

Aexpr *id_expr(string id) {
  Aexpr *s = (Aexpr *)checked_malloc(sizeof(*s));
  s->kind = Aidexpr;
  s->u.id = id;
  return s;
}

Aexpr *num_expr(int num) {
  Aexpr *s = (Aexpr *)checked_malloc(sizeof(*s));
  s->kind = Anumexpr;
  s->u.num = num;
  return s;
}

Aexpr *biop_expr(Aexpr *left, Abinop op, Aexpr *right) {
  Aexpr *s = (Aexpr *)checked_malloc(sizeof(*s));
  s->kind = Aopexpr;
  s->u.op.left = left;
  s->u.op.right = right;
  s->u.op.op = op;
  return s;
}

// it's just a cons list.
AexprList *exprlist_pair(Aexpr *first, AexprList *next_list) {
  AexprList *s = (AexprList *)malloc(sizeof(*s));
  s->kind = Apairlist;
  s->u.pair.head = first;
  s->u.pair.tail = next_list;
  return s;
}

AexprList *exprlist_last(Aexpr *last) {
  AexprList *s = (AexprList *)malloc(sizeof(*s));
  s->kind = AlastExprList;
  s->u.last = last;
  return s;
}

// this is not really typesafe..
Ast *node(void *o, AstTypes t) {
  Ast *ast = (Ast *)malloc(sizeof(*ast));
  ast->kind = t;
  assert(((Astm *)o)->kind);
  switch (t) {
  case AstAexpr:
    ast->u.expr = (Aexpr *)o;
    break;
  case AstAstm:
    ast->u.stm = (Astm *)o;
    break;
  case AstAexprList:
    ast->u.lst = (AexprList *)o;
    break;
  }
  return ast;
}

void free_astm(Astm *e) { visit_astm(e, free); }
void free_aexprlist(AexprList *e) { visit_aexprlist(e, free); }
void free_aexpr(Aexpr *e) { visit_aexpr(e, free); }

void visit_astm(Astm *e, void (*fn)(void *)) {
  switch (e->kind) {
  case AcompoundStm:
    visit_astm(e->u.compound.stm1, fn);
    visit_astm(e->u.compound.stm2, fn);
    break;
  case AassignStm:
    visit_aexpr(e->u.assign.expr, fn);
    break;
  case AprintStm:
    visit_aexprlist(e->u.print.exps, fn);
    break;
  }
  fn(e);
}

void visit_aexprlist(AexprList *e, void (*fn)(void *)) {
  switch (e->kind) {
  case Apairlist:
    visit_aexprlist(e->u.pair.tail, fn);
    visit_aexpr(e->u.pair.head, fn);
    break;
  case AlastExprList:
    visit_aexpr(e->u.last, fn);
    break;
  }
  fn(e);
}

void visit_aexpr(Aexpr *e, void (*fn)(void *)) {
  switch (e->kind) {
  case Aidexpr:
    break;
  case Anumexpr:
    // num is owned by expr, freed with free().
    break;
  case Aopexpr:
    visit_aexpr(e->u.op.left, fn);
    visit_aexpr(e->u.op.right, fn);
    break;
  case Aeseqexpr:
    visit_astm(e->u.eseqexpr.stm, fn);
    visit_aexpr(e->u.eseqexpr.expr, fn);
    break;
  }
  fn(e);
}

// pretty print the tree.
static void pretty_print(void *) { static int indent = 0; }
