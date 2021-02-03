val rent = 100
val negative = ~20
val diameter = 700 : int
val value = true : bool

(* ml list and characters *)
val bar = [#"H", #"e", #"l", #"l", #"o"]

val numbers = [1, 2, 3, 4, 5]

(* :: is cons ins tead of type signature *)
val numbers1 = 12 :: numbers

val guest_list = "Mon" :: "Dad" :: "Aund" :: []
val person = ("Person", 1)

(* record  {b:real, g:real, r:real}*)
val rgb = { r=0.32, g=0.56, b=0.91 }

val Hsv = {H=310.3, s=0.51, v=0.23}

(* sugar to get value out of records *)
val H = #H Hsv

(* define functions *)

fun thermometer temp =
  if temp < 37
  then "Cold"
  else if temp > 37
  then "Worm"
  else "Normal"

fun fib n =
  if n = 0 then 0 else
    if n = 1 then 1 else
      fib (n - 1) + fib (n - 2)

val x = fib 5

(* pattern matching *)
fun first (x::xs) = x;
fun second (x::y::xs) = y;

  (* pattern matching uses guard syntax in haskell... *)
fun on_even_idx (odd::even::xs) = even::on_even_idx xs
  | on_even_idx [odd] = []
  | on_even_idx [] = [];

  (* type alias*)
type loc = real * real

val locx = (1.1, 2.2) : loc

(* adt *)
datatype shape
  = Circle of loc * real
  | Square of loc * real
  | Trig of loc * loc * loc;

fun dist ((x0, y0), (x1, y1)) = let
  val dx = x1 - x0
  val dy = y1 - y0;
                                        in
                                          Math.sqrt (dx * dx + dy * dy)
end;

fun heron (a, b, c) = let
  val ab = dist (a, b)
  val bc = dist (b, c)
  val ac = dist (a, c)
  val perim = ab + bc + ac
  val s = perim / 2.0
in
  Math.sqrt (s * (s - ab) * (s - bc) * (s - ac))
end;

fun area (Circle (_, r)) = 3.14 * r * r
  | area (Square (_, s)) = s * s
  | area (Trig (a, b, c)) = heron (a, b, c); (* see above *)

  (* polymorphism *)
  (* one catch, to construct a record type you use the same
   * syntax as tuple.
   * (I think this makes more sense though, if records are really the same
   * as tuple...)
   * *)

datatype 'a btree = Leaf of 'a
                  | Node of 'a btree * 'a * 'a btree;

val mytree = Node (Leaf 3, 8, Node (Leaf 8, 5, Leaf 10));

fun count (Leaf n) = 1
  | count (Node (left, n, right)) = count left + 1 + count right;

count mytree
