(* Continuation: A function that express what to do next
 * *)

(* Though we have lots of anonymous values (lambdas, for example), what
 * the compiler really want is everything has a name and can be referred easily
 * by a table lookup. *)

(* CPS makes every part of control flow and data flow explicit. *)

module Prime = struct
  let rec iota l r : int list =
    l :: if l = r then [] else iota (l + 1) r
  exception E
  let isprime n =
    if n = 2 || n = 3 then true
    else if n < 2 || n mod 2 = 0 then false
    else
    let r = n |> float_of_int |> sqrt |> Float.floor |> int_of_float |> (fun x -> x + 1) in
    let result = ref true in
    (iota 2 r) |> (List.iter (fun x -> if n mod x = 0 then result := false else ()));
    !result
end


module DoPrimes = struct
(* Straight version
 * prodprimes has
 *   1. a return address k,
 *   2. a return value b of type bool,
 * in the second if clause we have two points:
 *   1. a point j: j p  = n * prodprimes (n - 1)
 *   2. a point h: h q = prodprimes (n - 1)
 * *)
let rec prodprimes n =
  if n = 1
  then 1
  else if Prime.isprime n
  then n * prodprimes (n - 1)
  else prodprimes (n - 1)

let isprime n k =  k (Prime.isprime n)

(* CPS transformed version
 * return just act like a function call.
 * Keep track of all control points it exposes.
 *
 * All control points are just functions, and all data labels are just
 * variables.
 * *)
let rec prodprimes' n c =
  if n = 1
  then c 1
  else
    let k = fun b ->
      if b = true
      then
        let j = fun p ->
          let a = n * p in c(a) in
        let m = n - 1 in
        prodprimes' m j
      else
        (* continuation being id indicates a tail call.
         * Because we can use c wherever h is used.
         * *)
        let h = fun q -> c(q) in
        let i = n - 1 in
        prodprimes' i h
    in isprime n k
end

(* CPS as IR has multiple advantages *)

(* NOTE: CPS for Inline expansion:
 * For CPS all paramters to functions are variables or constants,
 * never non trivial subexpression.
 * This avoid the problem the conflict of beta subsitution and strict evaluation
 * order.
 * *)

(* NOTE: CPS for closure reprensentation:
  * has nested scope in nature. It will be hard to represent the same notion with
  * say, SSA since there is no concept of nested scope in it's original form.
  * *)

(* NOTE: CPS for dataflow analysis:
 * We want the presentation shows the control flow faithfully.
 *)

(* NOTE: CPS for register allocation:
 * We want a good representation of the lifetime of a variable. (liveness analysis)
 * *)

let _ = DoPrimes.prodprimes' 10 (fun x -> Printf.printf "%d\n" x)
