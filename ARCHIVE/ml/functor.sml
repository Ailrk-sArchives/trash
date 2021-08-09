(* For modules we can also have values, types, and functions
 * *)

signature STACK =
sig
  type 'a stack
  exception EmptyStack

  val empty : 'a stack
  val isEmpty : 'a stack -> bool
  val push : ('a * 'a stack) -> 'a stack
  val pop : 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val map: ('a -> 'b)  -> 'a stack -> 'b stack
end

(* Functor takes a structure, return a new structure.
 * We can ensure the strucuture being passed in as parameter
 * implemented certain signature. It works like typeclass constraint.
 *
 * It's kind like higher kinded type.
 * *)

functor QueueFN (S:STACK) =
struct
  type 'a queue = 'a S.stack * 'a S.stack
  exception EmptyQueue

  val empty : 'a queue = (S.empty, S.empty)
  fun isEmpty ((s1, s2) : 'a queue) =
    S.isEmpty s1 andalso S.isEmpty s2

  fun enqueue (x : 'a, (s1, s2) : 'a queue) : 'a queue = (S.push (x, s1), s2)

  fun rev (s: 'a S.stack): 'a S.stack = let
    fun loop (old: 'a S.stack, new: 'a S.stack): 'a S.stack =
      if S.isEmpty old
      then new
      else loop (S.pop old, S.push(S.peek old, new)) in
        loop (s, S.empty)
  end

  fun dequeue ((s1, s2) : 'a queue) : 'a * 'a queue =
    if S.isEmpty s2
    then dequeue(S.empty, S.pop (rev s1)) handle S.EmptyStack => raise EmptyQueue
    else let
      val top = S.peek s2
      val q = (s1, S.pop s2 ) in
        (top, q) end

  fun map (f: 'a -> 'b) ((s1, s2) : 'a queue) : 'b queue =
    (S.map f s1, S.map f s2)
end

(* implement the stack *)
structure Stack :> STACK =
struct
  type 'a stack = 'a list
  exception EmptyStack

  val empty = [];
  fun isEmpty n = isEmpty n;
  fun push (n, xs) = n::xs;
  fun pop [] = raise EmptyStack
    | pop (x::xs) = xs

  fun peek [] = raise EmptyStack
    | peek (x::xs) = x

  fun map f [] = []
    | map f (x::xs) =
    let
      val v = f x
      val rest = map f xs in
        v::rest end
end

structure s = Stack;
structure queue1 = QueueFN(s);

signature ORDER = sig
  type element
  val compare : element * element -> order
end

functor BinarySearchTreeFn(O:ORDER) =
struct
  datatype 'a tree = Leaf
                   | Tree of 'a tree * 'a  * 'a tree
end

functor Dinctionary(O:ORDER) =
struct
  datatype ('a, 'b) dictionary = Dict of ('a * 'b) list
end
