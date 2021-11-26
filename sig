fun posToString s n = s ^ Int.toString(n);

structure Token :  IMPL_TOKEN =
struct
  type num = int
  type token = string

  fun ADD (pos, b) = posToString "ADD " pos
  fun SUB (pos, b) = posToString "SUB " pos
  fun MUL (pos, b) = posToString "MUL " pos
  fun AND (pos, b) = posToString "AND " pos
  fun OR (pos, b)  =  posToString "OR " pos
  fun EQ (pos, b)  =  posToString "EQ " pos

  fun IF (pos, b)    =  posToString "IF " pos
  fun THEN (pos, b)  =  posToString "THEN " pos
  fun ELSE (pos, b)  =  posToString "ELSE " pos
  fun WHILE (pos, b) =  posToString "WHILE " pos
  fun DO (pos, b)    =  posToString "DO " pos
  fun SKIP (pos, b)  =  posToString "SKIP " pos

  fun INT (c, pos, b) = "INT(" ^ Int.toString(c) ^ ") "  ^ ""
  fun ID (s, pos b) = "ID(" ^ s ^ ") " ^ Int.toString(pos)
  fun EOF (pos, b) = "EOF " ^ Int.toString(pos)
end
