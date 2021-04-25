(* -https://www.cs.cornell.edu/courses/cs3110/2011sp/Recitations/rec25-splay/splay.htm
 * *)

signature ORDERED_FUNCTIONAL_SET = sig
  type key
  type elem
  type set

  val compare: key * key -> order
  val keyOf : elem -> key
  val empty : unit -> set
  val add : set * elem -> set * bool
  val remove : set * key -> set * key option
  val lookup : set * key -> elem option
  val size : set -> int

  val first : set -> elem option
  val last : set -> elem option
  type 'b folder = ((elem * 'b) -> 'b) -> 'b -> key -> set -> 'b
  val fold_backward : 'b folder
  val fold_forward : 'b folder

  val print : set -> unit
end

signature ORDERED_SET_PARAMS = sig
  type key
  type elem
  val keyOf : elem -> key
  val compare : key * key -> order
  val toString : elem -> string
end


functor SplayTree(structure Params : ORDERED_SET_PARAMS)
  :>ORDERED_FUNCTIONAL_SET where type key = Params.key and
                                 type elem = Params.elem =
struct
  type key = Params.key
  type elem = Params.elem
  val compare = Params.compare
  val keyOf = Params.keyOf

  datatype tree = Empty
                | Node of tree * elem * tree

  type node = tree * elem * tree

end
