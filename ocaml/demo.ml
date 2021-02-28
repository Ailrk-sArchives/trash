open List;;

(* basic structures *)
let averate a b =
  let sum = a +. b in
  sum /. 20.0;;

let f a b =
  let x = a +. b in
  x +. x ** 2.0;;     (* common subexpression elimination *)

let v = List.length [1; 2; 3;];;

let tuple : (int * int * string) = (1, 2, "sd");;

(* define a reocord *)
