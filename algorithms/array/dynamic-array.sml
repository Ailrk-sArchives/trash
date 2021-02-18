(* Only two mutable strctures in sml:
 * ref and array. This is the same as haskell.
 * *)

signature MUTABLE_STACK =
sig
  type 'a mut_stack
  val new : unit -> 'a mut_stack
  val push : 'a * 'a mut_stack -> unit
  val pop : 'a mut_stack -> 'a option
end

structure MutableStack :> MUTABLE_STACK =
struct
  type 'a mut_stack = ('a list) ref
  fun new () = ref [];
  fun push (a, s) = s := a::(!s);
  fun pop s =
    case (!s) of
         [] => NONE
       | (x::xs) => (s := xs; SOME(x))
end

structure ArrayStack =
struct
  datatype 'a mut_stack = ArrayStack of {
    arr : 'a option Array.array ref,
    ptr : int ref
  };

  fun new () = ArrayStack {
    arr = ref (Array.array (256, NONE)),
    ptr = ref 0
    };

  (* bench mark version of push
   * Can return a result from this.
   * *)
  fun push_ factor (n_alloc : int ref option) =
    fn (a, ArrayStack {arr, ptr}) =>
      if (!ptr) < Array.length (!arr) - 1 orelse (!ptr) = 0
      then
        (ptr := (!ptr) + 1;
         Array.update (!arr, !ptr, SOME(a)))
      else
        let
          val len = Array.length (!arr);
          val ls = Array.array (floor ((Real.fromInt len) * factor), NONE);
        in
          (Array.appi (fn (i, x) => Array.update (ls, i, x)) (!arr);
           ptr := (!ptr) + 1;
           arr := ls;

           print (Int.toString (!ptr)
           ^ "reallocate, "
           ^ "new length: "
           ^ Int.toString(Array.length (!arr)) ^ "\n");

           case n_alloc of
                NONE => ()
              | SOME(r) => r := (!r) + 1;
           Array.update (ls, !ptr, SOME(a)))
        end

  (* be careful about recursive polymorphic types *)
  fun push (a, b) = (push_ 1.5 NONE) (a, b);

  fun pop (ArrayStack {arr, ptr}) : 'a option =
    if (!ptr) = 0
    then NONE
    else
      let val head = Array.sub (!arr, !ptr);
      in (ptr := (!ptr) - 1; head) end
end

structure Test =
struct
  val s = ref (ArrayStack.new () : int ArrayStack.mut_stack);
  val n_alloc = SOME(ref 0);
  val seed = Random.rand (123, 234)

  fun clean () =
    (s := (ArrayStack.new () : int ArrayStack.mut_stack);
     case n_alloc of
          NONE => ()
        | SOME(r) => r := 0)

  fun test_push factor n =
      if n > 0
      then
        let
          val push = ArrayStack.push_ factor n_alloc;
        in (push (n, !s); test_push factor (n - 1)) end
      else
        case n_alloc of
             NONE => ()
           | SOME (r) =>
               print ("Total allocation: "
               ^ (Int.toString (!r))
               ^ "\n");

  fun test_pop n =
    if n > 0
    then (ArrayStack.pop (!s); test_pop (n - 1))
    else ()

  (* We want to check in a series of random operations, how frequent
   * allocation can be.
   * *)

  fun push_random factor =
    let
      val n = Random.randRange (0, 1000000) seed
    in
      (print "start\n";
       test_push factor n;
       print "end\n";
       clean ())
    end
end
