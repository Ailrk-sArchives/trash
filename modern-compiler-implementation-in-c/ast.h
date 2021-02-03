#pragma once
#include "util.h"
#include <stdbool.h>
#include <stdio.h>

// BFN
// Stm -> Stm ; Stm
// Stm -> id := Expr
// Stm -> print (ExprList)
// Expr -> id
// Expr -> num
// Expr -> (Stm, Expr)
// ExprList -> Expr, ExprList
// ExprList -> Expr
// Binop -> +
// Binop -> -
// Binop -> *
// Binop -> /

// even writing in c you simulate the ast with struct and union.
typedef enum { Aplus, Aminus, Atimes, Adiv } Abinop;

struct Astm_;
struct AexprList_;
struct Aexpr_;

typedef struct Astm_ Astm;
typedef struct AexprList_ AexprList;
typedef struct Aexpr_ Aexpr;

void free_astm(Astm *);
void free_aexprlist(AexprList *);
void free_aexpr(Aexpr *);

void visit_astm(Astm *, void(*fn)(void *));
void visit_aexprlist(AexprList *, void(*fn)(void *));
void visit_aexpr(Aexpr *, void(*fn)(void *));

typedef enum AstmTypes { AcompoundStm, AassignStm, AprintStm } AstmTypes;
struct Astm_ {
  AstmTypes kind;
  union {
    struct {
      Astm *stm1;
      Astm *stm2;
    } compound;

    struct {
      string id;
      Aexpr *expr;
    } assign;

    struct {
      AexprList *exps;
    } print;
  } u;
};
void free_1(Astm *);

Astm *compound_stm(Astm *s1, Astm *s2);
Astm *assign_stm(string id, Aexpr *expr);
Astm *print_stm(AexprList *exps);

typedef enum { Aidexpr, Anumexpr, Aopexpr, Aeseqexpr } AexprTypes;
struct Aexpr_ {
  AexprTypes kind;
  union {
    char *id;
    int num;
    struct {
      Aexpr *left;
      Abinop op;
      Aexpr *right;
    } op;

    struct {
      Astm *stm;
      Aexpr *expr;
    } eseqexpr;
  } u;
};

Aexpr *id_expr(string id);
Aexpr *num_expr(int num);
Aexpr *biop_expr(Aexpr *left, Abinop op, Aexpr *right);

typedef enum { Apairlist, AlastExprList } AexprListTypes;
struct AexprList_ {
  AexprListTypes kind;
  union {
    struct ast_t {
      Aexpr *head;
      AexprList *tail;
    } pair;
    Aexpr *last;
  } u;
};

AexprList *exprlist_pair(Aexpr *first, AexprList *next_list);
AexprList *exprlist_last(Aexpr *last);

typedef enum { AstAexpr, AstAstm, AstAexprList } AstTypes;
// A uniform interface for all notes;
// This serves as a fat pointer to handle the entire tree.
typedef struct {
  AstTypes kind;
  union { // unions holds the access of the underlying ast node.
    Aexpr *expr;
    Astm *stm;
    AexprList *lst;
  } u;
} Ast;
