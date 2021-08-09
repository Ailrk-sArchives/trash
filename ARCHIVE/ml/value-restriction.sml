(* Some quirks with sml's type system. sml's type system has been
 * like this since sml 97, so it doesn't have all the fancy support
 * that haskell provides.
 * For instance, there is no rankNtype, and there is these value restruction
 * because of imperative code.
 * *)


structure Restriction =
struct
(* Hidley milner let's sml infer types easily
 * We can tell map : ('a list -> 'b list) -> 'a list -> 'b list
 * right away.
 * *)
  fun map _ [] = []
    | map f (x::xs) = (f xs) :: map f xs;


  (* but there is no subtyping, e.g int and real are different
   * *)

  (* no overload from types, this functions only works for one type
   * (like monomorphization)
   * *)
  fun sqr n = n * n;

  (* equality doesn't work for some types (namely types with function) *)
  fun member (k, []) = false
    | member (k, x::xs) = (x = k) orelse member (k, xs);

  (* no rank N type TAT *)

  (* and the value restriction because of imperative code
   * Basically value restriction means you can only have your
   * syntatic types to be infered as polymorphic.
   * Types like literals, fn, and constructors are ok, but you can't
   * infer types for record. (note fn as lambda)
   * You need to use eta expansion to make expression into a context to
   * allows it to be polymorphic. Example:
   * *)

   val revlists' = map rev; (* in this case it doesn't know what rev is *)

   val revlists = (fn xs => map rev xs); (* works wit eta expansion. *)

  (* The reason why you need value restriction is because sml support imperative
   * programming, and that makes it possible to instantiate a same type
   * parameter in different steps, and violate type safty.
   * *)

   (* a harmless oo example that keeps the type safty *)
    val account =
      let
        val bal = ref 1000 in
          { deposit = fn d => bal := !bal + d,
            balance = fn () => !bal
          }
      end

    (* but this one allows you to do something wacky *)
  val stack =
    let
      val stk = ref [] in
        { push = fn x => stk := x :: !stk,
          pop = fn () => stk := tl (!stk),
          top = fn () => hd (!stk)
        }
    end;

    (* you can push a int and use it as a bool.... *)

end

(* http://users.cs.fiu.edu/~smithg/cop4555/valrestr.html *)
(* https://jozefg.bitbucket.io/posts/2015-03-27-unsafe.html *)
