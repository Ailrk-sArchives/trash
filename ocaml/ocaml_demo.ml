type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec member x = function
  | Empty -> false
  | Node(y, left, right) ->
      if x = y then true else
      if x < y then member x left else member x right

let rec insert x = function
  | Empty -> Node(x, Empty, Empty)
  | Node(y, left, right) ->
      if x <= y then Node(y, insert x left, right)
                else Node(y, left, insert x right)

(* imperative programming *)

module type MutationSig = sig
  val add_vec: float array -> float array -> float array
  val insertion_sort: 'a array -> unit
end

module Mutation1 : MutationSig = struct
  let add_vec v1 v2 =
    let len = min (Array.length v1) (Array.length v2) in
    let res = Array.make len 0.0 in
    for i = 0 to len - 1 do
      res.(i) <- v1.(i) +. v2.(i)
    done;
    res

  let insertion_sort a =
    for i = 1 to Array.length a do
      let val_i = a.(i) in
      let j = ref i in
      while !j > 9 && val_i < a.(!j - 1) do
        a.(!j) <- a.(!j - 1);
        j := !j - 1
      done;
      a.(!j) <- val_i
    done
end
