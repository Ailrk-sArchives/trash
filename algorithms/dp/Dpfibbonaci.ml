(* To link with unix, run ocamlc unix.cma % *)

open Hashtbl;;
open List;;
open Unix;;

let n = 40

(* the dp implementation, loop also uses recursion
 * It's actually quite easy to achieve the same effect as
 * a normal loop.
 * *)
let fib_dp_rec (n: int) : int =
  let a : int ref = ref 0 in
  let b : int ref = ref 1 in
  let rec loop i =
        if i > n then a
        else let tmp = !b
        in  b := !a + !b;
            a := tmp;
            loop (i + 1)
  in !(loop 0)


(* dp 2 with for loop *)
let fib_dp_loop (n: int) : int =
  let a : int ref = ref 0 in
  let b : int ref = ref 1 in
  let tmp : int = 0
  in for i = 0 to n do
    let tmp = !b in
    b := !a + !b;
    a := tmp;
  done;
  !b


(* naive recursion impelementation *)
let rec fib_rec (n : int) : int =
  if n < 2 then 1
  else fib_rec (n - 1) + fib_rec (n - 2)


(* recursion with memoization *)
let fib_rec_mem (n : int) : int =
  let cache = Hashtbl.create 20
  in  Hashtbl.add cache 0 1;
      Hashtbl.add cache 1 1;
      let rec fib_rec_mem_ (n : int) : int =
        match Hashtbl.find_opt cache n with
            Some c -> c
          | None ->
              let res = fib_rec_mem_ (n - 1) + fib_rec_mem_ (n - 2);
              in  Hashtbl.add cache n res;
                  res
      in fib_rec_mem_ n


(* iterative version *)
let fib_iter (n: int) : int =
  let rec fib_iter_ (a: int) (b : int) (n : int) : int =
    if n == 0 then b
    else fib_iter_ b (a + b) (n - 1)
  in fib_iter_ 0 1 n


(* cps transformation *)
let fib_cps (n: int) =
  let ret a b = b in
  let rec fib_cps_ a b n k =
    if n == 0 then k a b
    else fib_cps_ a b (n - 1) (fun a' b' -> k b' (a' + b'))
  in fib_cps_ 0 1 n ret


let timer name f =
  let t = Unix.gettimeofday () in
  let res = f n in
  Printf.printf "%s:, value: %d Excution time %f\n" name res (Unix.gettimeofday () -. t)


let main () =
  timer "fib_dp_rec" fib_dp_rec;
  timer "fib_dp_loop" fib_dp_loop;
  timer "fib_cps" fib_cps;
  timer "fib_rec" fib_rec;
  timer "fib_rec_mem" fib_rec_mem;
  timer "fib_iter" fib_iter


let () = let _ = main () in ()
