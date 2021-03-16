(* test some continuations *)

(* straightforward continuation *)
let rec sum1 s =
  match s with
      [] -> 0
    | x::xs -> x + (sum1 xs)

(* tail recursion *)
let rec sum2 s =
  let rec sum' s a =
    match s with
        [] -> a
      | x::xs -> sum' xs (a + x) in
  sum' s 0

(* using continuation *)
(* (\a2 (\a1 -> (\a0 -> (id 0) + a0) (x1 + a1)) (x2 + a2)) x3
 * => (\a1 -> (\a0 -> (id 0) + a0) (x1 + a1)) (x2 + x3)
 * => (\a0 -> (id 0) + a0) (x1 + (x2 + x3))
 * => (id 0) + (x1 + (x2 + x3))
 * => 0 + x1 + x2 + x3
 *
 * which is equivalent to
 * (\x -> x) . (\a -> x1 + a) . (\x -> x2 + a)l ...
 *
 * Although it's tail recursion, it needs to remember the closure of the
 * previous continuation, so it doesn't really save any sapce comparing with
 * straight recurison.
 *)
let rec sum3 s =
  let rec sum' s k =
    match s with
        [] -> k 0
      | x::xs -> sum' xs (fun a -> k (x + a))
  in sum' s (fun x -> x)

  (* stack overflow direclty *)
let rec inf = 0::inf in
  sum1 inf

  (* no problem *)
let rec inf = 0::inf in
  sum2 inf

  (* Will not stack overflow at least.
   * but will keep running until no more memory left
   * *)
let rec inf = 0::inf in
  sum3 inf


(* some more examples *)

let rec fold_right1 (f : 'a -> 'b -> 'b) (s : 'a list) (b : 'b) : 'b =
  match s with
     [] -> b
   | x::xs -> f x (fold_right1 f xs b)

(* no stack allocatin at all, since they are tail eliminated.
 * But now we have to store the closure somewhere.
 * It's still deferred calculation.
 * *)
let rec fold_right2 (f : 'a -> 'b -> 'b) (s : 'a list) (b : 'b) : 'b =
  let rec fold_right' s k =
    match s with
       [] -> k b
     | x::xs -> fold_right' xs (fun a -> k (f x a))
  in fold_right' s (fun x -> x)
