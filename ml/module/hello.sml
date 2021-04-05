(* mlton wants every files has a module to export.
 * In sml, files means very little. the logical separations
 * between different parts of the program is defined by different
 * modules.
 * You can think mlton will conceptually combine all files  together
 * before compile, so at compile time it will know the existence of all
 * modules.
 *
 * Of course this doesn't work for separate compilation, which is a good thing
 * to persue. We want to be able to compile only part of the dependency graph
 * when there are changes.
 *
 * This is why we also have cm, compilation manager to help us to do incremental
 * compilation.
 * *)
structure Hello =
struct
  fun hello () = print "Hello, world\n";
  fun main () = hello ();

  (* nested  structure definition *)
  structure Math =
  struct
    fun add (a, b) = a + b;
    fun minus (a, b) = a - b;
    fun multiply (a, b) = a * b;
    fun divide (a, b) = a / b;
  end

  structure Math1 =
  struct
    fun sqrt x = let open Math in multiply (x, x) end
    fun cube x = Math.multiply (x, Math.multiply (x, x))
    end

    (* You still have currying, but people like to use tuple for
     * some reason.
     * *)
  fun add1 a b = a + b;

  structure Dog =
  struct
    datatype dogs = Hasky
                  | Shiba
                  | Chiwawa;
    fun bark Hasky = "Woof"
      | bark Shiba = "Wooof"
      | bark Chiwawa = "wooc";

  end
  end

  signature MAP =
  sig
    type key
    type 'a table

    val empty : 'a table
    val insert : key -> 'a -> 'a table -> 'a table
    val lookup : key -> 'a table -> 'a option
end


(* We defined a signature for queue.
 * You can think it as a typeclass with type family + data family maybe?
 * *)
 signature QUEUE =
 sig
   type 'a queue
   exception QueueError
   val empty : 'a queue
   val isEmpty : 'a queue -> bool
   val singleton : 'a -> 'a queue
   val insert : 'a * 'a queue -> 'a queue
   val peek : 'a queue -> 'a
   val remove : 'a queue -> 'a * 'a queue
  end

  (* Provide an implementation *)
structure TwoListQueue :> QUEUE =
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

 functor BFS (structure Q: QUEUE) = (* after Okasaki, ICFP, 2000 *)
 struct
   datatype 'a tree = E
                    | T of 'a * 'a tree * 'a tree

   fun bfsQ (q  : 'a tree Q.queue)  : 'a list =
     if Q.isEmpty q
     then []
     else let val (t, q') = Q.remove q in
       case t of E => bfsQ q'
          | T (x, l, r) => let
            val q'' = Q.insert (r, Q.insert (l, q')) in
              x  :: bfsQ q'' end end
   fun bfs t = bfsQ (Q.singleton t)
  end
