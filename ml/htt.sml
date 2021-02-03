signature QUEUE =
sig
  type 'a queue
  exception QueueError
  val empty     : 'a queue
  val isEmpty   : 'a queue -> bool
  val singleton : 'a -> 'a queue
  val insert    : 'a * 'a queue -> 'a queue
  val peek      : 'a queue -> 'a
  val remove    : 'a queue -> 'a * 'a queue
end

structure TwoLisQueue :> QUEUE =
struct
  type 'a queue = 'a list * 'a list
  exception QueueError

  val empty = ([], [])
  fun isEmpty ([], []) = true
    | isEmpty _ = false

  fun singleton a = ([], [a])

  fun insert (a, ([], [])) = ([], [a])
    | insert (a, (ins, outs)) = (a::ins, outs)

  fun peek (_, []) = raise QueueError
    | peek (ins, a::outs) = a

  fun remove (_, []) = raise QueueError
    | remove (ins, [a]) = (a, ([], rev ins))
    | remove (ins, a::outs) = (a, (ins, outs))
end
