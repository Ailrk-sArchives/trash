/* the book list some quirks of c */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void compatible_ptr() {
  char **ptr1 = NULL;
  const char **ptr2 = NULL;
  /* it's an error because of some rules of ptr assignment.
   * A ptr can assign to another ptr only when
   *  1. they both point to the compatible type (same type)
   *  2. left operand has all the qualifiers of the right operand
   * But is this case, ptr1 is a pointer to "pointer to char (char *)",
   * yet ptr2 is a pointer to "point to const char (const char *)".
   * So they point to different types technically. Thus they are not
   * ptrs to the compatible type. Rule 1 violated.
   *
   * It is so sketchy to be honest.
   * */
  ptr1 = ptr2;
}

void unsigned_conversion() {
  /* Usual arithmetic conversion.
   * In expression you try to convert a type to the "biggest"
   * form. (be as float as possible, as long as possible, as
   * unsigned as possible etc..)
   *
   * Some quriks will happen with expression involve float and
   * unsigned. Cause the sign flipped the evaluation can change
   * to something completely different.
   * */
  if (-1 < (unsigned char)1) { /* this print "1".*/
    printf("1");
  } else {
    printf("0");
  }

  /* this print "oops"...
   * Cause sizeof return unsigned value, and the arithmetic
   * conversion convert signed int -1 to unsigned int.
   * Need to be extra careful when unsigned int is involved */
#define TOTAL_ELEMENTS (sizeof(array) / sizeof(array[0]))
  const int array[] = {10, 20, 30, 40, 50, 60};
  int d = -1;
  if (d <= TOTAL_ELEMENTS - 2) {
    printf("yes");
  } else {
    printf("oops");
  }

#undef TOTAL_ELEMENTS
}

/* cdecl */
#define MAXTOKENS 100
#define MAXTOKENLEN 64

typedef enum type_tag { IDENTIFIER, QUALITFIER, TYPE } TypeTag;
typedef struct token {
  char type;
  char string[MAXTOKENLEN];
} Token;

static int top=-1;
static Token stack[MAXTOKENS];
static Token self;

#define pop stack[top--]
#define push(s) stack[++top] = s

TypeTag classify_string() {
  char *s = self.string;
  if (!strcmp(s, "const")) {
    strcpy(s, "read-only");
    return QUALITFIER;
  }

  if (!strcmp(s, "volatile"))
    return QUALITFIER;
  if (!strcmp(s, "void"))
    return TYPE;
  if (!strcmp(s, "char"))
    return TYPE;
  if (!strcmp(s, "signed"))
    return TYPE;
  if (!strcmp(s, "unsigned"))
    return TYPE;
  if (!strcmp(s, "short"))
    return TYPE;
  if (!strcmp(s, "int"))
    return TYPE;
  if (!strcmp(s, "float"))
    return TYPE;
  if (!strcmp(s, "double"))
    return TYPE;
  if (!strcmp(s, "struct"))
    return TYPE;
  if (!strcmp(s, "union"))
    return TYPE;
  if (!strcmp(s, "enum"))
    return TYPE;
  return IDENTIFIER;
}

void gettoken() {
  char *p = self.string;
  while ((*p = getchar()) == ' ')
    ;
  if (isalnum(*p)) {
    while (isalnum(*++p=getchar()))
      ;
    ungetc(*p, stdin);
    *p  = '\0';
    self.type = classify_string();
    return;
  }

  if (*p == '*') {
    strcpy(self.string, "pointer to");
    self.type = '*';
    return;
  }

  self.string[1] = '\0';
  self.type = *p;
  return;
}

void read_to_first_identifier() {
  gettoken();
  while (self.type != IDENTIFIER) {
    push(self);
    gettoken();
  }
  printf("%s is", self.string);
  gettoken();
}

void decarrays() {
  while (self.type=='[') {
    printf("array ");
    gettoken();
    if (isdigit(self.string[0])) {
      printf("0..%d", atoi(self.string) - 1);
      gettoken();
    }
    gettoken();
    printf("of ");
  }
}

void decfunctionargs() {
  while (self.type != ')') {
    gettoken();
  }
  gettoken();
  printf("function returning");
}

void decpointers() {
  while (stack[top].type == '*') {
    printf("%s", pop.string);
  }
}

void declarator() {
  switch (self.type) {
    case '[':
      decarrays();
      break;
    case '(':
      decfunctionargs();
      break;
  }
  decpointers();

  while (top >= 0) {
    if (stack[top].type == '(') {
      pop;
      gettoken();
      declarator();
    } else {
      printf("%s ", pop.string);
    }
  }
}

void cdecl() {
  read_to_first_identifier();
  declarator();
  printf("\n");
}
#undef MAXTOKENS
#undef MAXTOKENLEN

int main(void) { return 0; }
