signature IMPL_TOKEN =
sig
  type num
  type token
  val ADD: num * num -> token;
  val SUB: num * num -> token;
  val MUL: num * num -> token;
  val AND: num * num -> token;
  val OR: num * num -> token;
  val EQ: num * num -> token;

  val IF: num * num -> token;
  val THEN: num * num -> token;
  val ELSE: num * num -> token;
  val WHILE: num * num -> token;
  val DO: num * num -> token;
  val KKIP: num * num -> token;

  val INT: int * num * num -> token:
  val ID: string * num * num -> token:
  val EOF: num * num -> token:
end
