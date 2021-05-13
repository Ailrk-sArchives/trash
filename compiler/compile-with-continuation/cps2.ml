module CPS = struct
  type var
  type value =
    Var of var
  | Label of var
  | Int of int
  | Real of string
  | String of string

  type accesspath =
    Off of int
  | Sel of int * accesspath

  type primop = Add | Minus | Mul | Div | Gt | Lt | Ge | Le | Eq | Ne

  type cexp =
    Record of (value * accesspath) list * var * cexp
  | Select of int * value * var * cexp
  | Offset of int * value * var * cexp
  | App of value * value list
  | Fix of (var * var list * cexp) list * cexp
  | Switch of value * cexp list
  | Primop of primop * value list * var list * cexp list
end

let un = assert false

(* a example of representing addition with cps
 * Notes:
 *    1. all sub expressions must have a name. (arguments are atomic)
 * *)
let add_example () =
  let c: CPS.var = un in let d: CPS.cexp = un in let x: CPS.var = un in let y: CPS.var = un in
  CPS.Primop (CPS.Add, [CPS.Var x; CPS.Var y], [c], [d])

(* continuation implies an order of evaluation.
 * For the example below we have (x + y) * (10 * z)
 * You can evaluate (x + y) first, or (10 * z) first.
 * CPS forece you to choose one or the other when you're doing the
 * transfortation. *)
let nested_add () =
  let m: CPS.cexp = un in
  let a: CPS.var = un in let b: CPS.var = un in let c: CPS.var = un in
  let x: CPS.var = un in let y: CPS.var = un in let z: CPS.var = un in
  CPS.Primop (CPS.Add, [CPS.Var x; CPS.Var y], [a],
  [CPS.Primop (CPS.Add, [CPS.Int 10; CPS.Var z], [b],
  [CPS.Primop (CPS.Mul, [CPS.Var a; CPS.Var b], [c], [m])])])

(* this continuation doesn't return value, but choose a continuation to call based on
 * the result *)
let cps_with_no_output () =
  let a: CPS.var = un in let b: CPS.var = un in let f: CPS.cexp = un in let g: CPS.cexp = un in
  CPS.Primop (CPS.Gt, [CPS.Var a; CPS.Var b], [], [f; g])

(* Given a value v, choose one of the branch continution to preceed
 * e.g v = 1 choose e1 etc.  *)
let multiway_branches () =
  let v: CPS.value = un in let e0: CPS.cexp = un in let e1: CPS.cexp = un in let e2: CPS.cexp = un in
  CPS.Switch (v, [e0; e1; e2])

(* record operations
 * select + set. Row poly extends notions here.
 * *)

(* use continuation to represent record *)
let records () =
  let a: CPS.var = un in let c: CPS.var = un in let u: CPS.cexp = un in
  CPS.Record([(CPS.Var a, CPS.Off 0); (CPS.Int 2, CPS.Off 0)], c,  u)

(* select the 1th field from record u and bind the result to v, continue with
 * select the 2th field from record u and bind the result to w, continue with  e.
 * *)
let select_from_records () =
  let u: CPS.var = un in let v: CPS.var = un in let w: CPS.var = un in let e: CPS.cexp = un in
  CPS.Select (1, CPS.Var u, v,
  (CPS.Select (2, CPS.Var u, w, e)))

(* offet allows a varaible to point to the middle of a reacord
 * It has the same argument as select, but the purpose is to adjust the pointer.
 *)
let offset_of_records () =
  let v: CPS.var = un in let w: CPS.var = un in let e: CPS.cexp = un in
  CPS.Offset (1, CPS.Var v, w, e)

(* mutually recursive functions are defined with fix point *)
let mutually_recursive () =
  let f1: CPS.var = un in let v11: CPS.var = un in let v12: CPS.var = un in let b1: CPS.cexp = un in
  let f2: CPS.var = un in let v21: CPS.var = un in let v22: CPS.var = un in let b2: CPS.cexp = un in
  let e: CPS.cexp = un in
  CPS.Fix ([(f1, [v11; v12], b1);
            (f2, [v21; v22], b2)], e)

(* in CPS all functoini calls are tail calls
 * It's clear to write is as type, App has no cexp field!
 * *)
let apps_are_tailcalls () =
  let f: CPS.var = un in let v1: CPS.var = un in let v2: CPS.var = un in
  CPS.App (CPS.Var f, [CPS.Var v1; CPS.Var v2])


(* tansform *)
let _f () =
  let f x = 2 * x + 1
  in f (11 + 22) * f (33 + 44)

(* r is the rest of the entire computation *)
let _f_cps r =
  let f x k = k (2 * x + 1) in  (* we have this variable k determines return *)
  let k1 = fun i ->
    let k2 = fun j ->
      r (i * j)         (* all subexpressions are given a name *)
    in f (33 + 44) k2
  in f (11 + 22) k1     (* pass the result to continuaiont k1. *)
  (* k1 is also defined by us *)

(*
 * compile to CPS datatype
 * *)


