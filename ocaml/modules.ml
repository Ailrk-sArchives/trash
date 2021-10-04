(* all code are wrapped into a module.
 * a module itself can be a submodule of another module.
 * *)


(* ---------------------------------------------------------------------------
   Hello with only struct *)
module Hello = struct
  let message = "Hello"
  let hello () = print_endline message
end

let goodbye () = print_endline "Goodbye"

let hello_goodbye () =
  Hello.hello ();
  goodbye ()

(* ---------------------------------------------------------------------------
Hi with signature *)
module type HI = sig
  val hi : unit -> unit
end

module Hi : HI = struct
  let message= "Hi"
  let hi () = print_endline message
end

let hi_goodbye () =
  Hi.hi ();
  goodbye ()


(* playing around with it
 * Really plain old module is just a standalone namespace.
 * *)
module M = struct let x = 43 end

module P = struct
  open M
  let y = x
end

let x = M.x;;
let y = P.y;;

module B1 = struct
  let x = 42

  (* idiomatic way to expose the main type *)
  type t = bool

  (* you can define an exception *)
  exception E

  (* above is the same as this
   * type _ += .. is extensible variants. it allows you to extend
   * the type exn (a extensible variant) with a new constructor E1.
   * *)
  type exn += E1 = E
  module N = struct
    let y = 0
  end
end

(* ---------------------------------------------------------------------------
   Use interface to define abstract type.
   definitions for signature *)
module type Sig = sig
  val f : int -> int
end

module MSig1 : Sig = struct
  let f x = x + 1
end

module MSig2 : Sig = struct
  let f x = x
end


(* ---------------------------------------------------------------------------
   module type
   used to describe groups of related modules.
 *)

module type Stack = sig
  type 'a stack
  val empty : unit -> 'a stack
  val is_empty : 'a stack -> bool
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
end

(* implement Stack sig *)
module MyStack : Stack = struct
  type 'a stack =
    | Empty
    | Entry of 'a * 'a stack

  let empty () = Empty
  let is_empty s = s = Empty
  let push x s = Entry (x, s)
  let peek = function         (* lambda cases *)
    | Empty -> failwith "Empty"
    | Entry (x,_) -> x
  let pop = function
    | Empty -> failwith "Empty"
    | Entry (_,s) -> s
end

module ListStack : Stack = struct
  type 'a stack = { mutable elements : 'a list }
  let empty () = { elements = [] }
  let is_empty s = s.elements = []
  let push x s = s.elements <- x::s.elements; s
  let pop s =
    match s.elements with
      _::t -> s.elements <- t; s
    | [] -> failwith "Empty"
  let peek s =
    match s.elements with
      h::_ -> h
    | [] -> failwith "Empty"
end


(* ---------------------------------------------------------------------------
   abstract data type with module.
   example, don't mix cashes (although they are all floats)
*)

module type Currency = sig
  type t            (* this can be anything *)
  val unit : t
  val plus : t -> t -> t
  val prod : float -> t -> t
end

module Float = struct
  type t = float
  let unit = 1.0
  let plus = (+.)
  let prod = ( *. )
end

(* bind the implementation later. This allows us to have same implementatoin
   but different names. It's essentially new type warpper. *)
module Euro = (Float : Currency)
module Dollar = (Float : Currency)

let euro x = Euro.prod x Euro.unit
let dollar x = Dollar.prod x Dollar.unit


(* Functor *)

module type T = sig  (* first define a signature *)
  type t
  val x : t
  val g : t -> t
  val to_string : unit -> string
end

module S1 : T = struct  (* one possible implementation *)
  type t = float
  let x = 10.1
  let g x = x +. 1.
  let to_string () = Format.sprintf "%f" (g x)
end

module S2 : T = struct  (* another possible implementation *)
  type t = int
  let x = 10
  let g x = x + 1
  let to_string () = Format.sprintf "%d" (g x)
end

module F = functor(X : T) ->  (* functor to produce new module from any T *)
  struct
    type u = X.t * X.t
    let y = X.g (X.x)
    let foo (s : int) = Printf.printf "parameter number is %d, X.x is %s" s (X.to_string ())
  end

module FS1 = F(S1)
module FS2 = F(S2)


(* submodule interface *)
module SubModuleHello : sig
 val hello : unit -> unit
end
=
struct
  let message = "Hello"
  let hello () = print_endline message
end
