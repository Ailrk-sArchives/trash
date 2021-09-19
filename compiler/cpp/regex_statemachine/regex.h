#pragma once
#include <stdio.h>

// regular expression is just a text encoding of finite state machine. One can
// mechanically construct a FSM with a given regular expression.

// regular expression that only implements klenee algebra
// (C, +, *)

// FSM is a table driven machine, to know what to do next we only need to know
// two things: 1. the current state 2. the current symbol.
// essentially we use this two information to next in the table for the next
// action.

// compiling regular expresion thus becomes generating the table.

typedef struct node_ {
  char s;
  struct node_ *left;
  struct node_ *right;
} node;


int regex(char pat[], char str[]);
