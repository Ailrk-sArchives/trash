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

(* named argument *)
let f1 ~name1:arg1 ~name2:arg2 = arg1 + arg2
let f2 ~name1 ~nam2 = name1 + name2
let f3 ~name1:(arg1:int) ~name2:(arg2:int) = arg1 + arg2
let f4 ?name:(arg1=9) arg2 = arg1 + arg2

let square a = a * a

let sum xs = List.fold_left (+) 0 xs

(* reverse function composition *)
let sum_sq n =
  [1;2;3;4;5]
  |> List.rev_map square
  |> sum
