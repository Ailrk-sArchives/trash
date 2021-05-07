(* SECD invented by john peter ladin in 1964
 * stands for Stack Environment Control Dump.
 * It's an stack based abstract machine intended as a
 * target for * functional languages compilers.
 *
 *
 * *)



module type Stack = sig
  type 'a t
  val empty : unit -> 'a t
  val is_empty : 'a t -> bool
  val pop : 'a t -> 'a t
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
end

(* presistent stack *)
module ListStack = struct
  type 'a t = 'a list
  let empty () = []
  let is_empty s = s = []
  let pop = function
    | x::xs -> xs
    | [] -> failwith "Empty"
  let push x s = x::s
  let peek = function
    | x::xs -> x
    | [] -> failwith "Empty"
end

module L = ListStack
