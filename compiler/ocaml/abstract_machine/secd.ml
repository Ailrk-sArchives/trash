(*
  SECD machine is a abstract machine for call-by-value semantics. It was first
  invented by Peter J Ladin in 1964 in his paper mechanical evaluation of
  expressions.

  Original SECD machine had four stacks: Stack, environment, code, dump. Modern
  SECD machine simplify the design and reduce the amount of stacks required
  to three. Namely we only need S: stack, E: environment, C: code. D: Dump was
  used for implementing function calls, but we can just use stack to do that
  alreay.
 *)


module SECDMachineNaive = struct
  (* A naive SECD machine that just implements the base line semantics.
     environments for closure replace bound variables.
     We use deburjin index for representing variables. This way we don't need
     to worry about name capturing.
   *)

  exception Error

  (* Note the debrujin index starts from 0 *)
  type inst =
    | LDV of value
    | ACCESS of int
    | CLOSURE of inst list
    | LET
    | ENDLET
    (* function calls *)
    | APPLY
    | RETURN

    (* basic arithmetics*)
    | ADD
    | SUB
  and value =
    | VInt of int
    | VClos of inst list * value list
    | VEnv of value list
    | VInst of inst list

    (* debugging case *)
    | VUnknown of inst list * value list * inst list

  type environment = value list

  let interpreter code =
    let stk = [] in
    let (env: environment) = [] in
    let rec loop s e c = match s, e, c with
      | s, e, LDV n::cs -> loop (LDV n::s) e cs
      | LDV(VInt(a))::LDV(VInt(b))::s, e, ADD::cs ->
          loop (LDV(VInt(a + b))::s) e cs

      | LDV(VInt(a))::LDV(VInt(b))::s, e, SUB::cs ->
          loop (LDV(VInt(a - b))::s) e cs

      | s, e, ACCESS(n)::cs -> loop (LDV(List.nth e n)::s) e cs
      | (LDV v::s), e, LET::cs -> loop s (v::e) cs
      | s, (_::e), ENDLET::cs -> loop s e cs
      | s, e, CLOSURE c'::cs -> loop (LDV(VClos(c', e))::s) e cs

      | ((LDV v)::LDV(VClos(c', e'))::ss), e, (APPLY::cs) ->
          loop (LDV(VInst(cs))::(LDV(VEnv e))::ss) (v::e') c'

      | (v::(LDV(VInst(c)))::(LDV(VEnv e'))::ss), _, RETURN::_ ->
          loop (v::ss) e' c

      | (LDV(v)::_), _, [] -> v
      | s, e, c -> VUnknown (s, e, c)
    in loop stk env code
end

module TestSECDMachineNaive = struct
  open SECDMachineNaive
  let t1 () =
    let open SECDMachineNaive in
    let p = [CLOSURE [ACCESS(0); LDV(VInt 2); ADD; RETURN]; LDV(VInt 1); APPLY]
    in interpreter p
end

(* The idea is simple:
   f = \. .. g 1 ..
   g = \. h(..)
   h = \. ..
   once g calls h, stack for g essentially useless, because when h returns
   there is nothing more for g to do.

   But there are some implication for this technique.
*)
module SECDTailCalled = struct
  exception Error

  type inst =
    | LDV of value
    | ACCESS of int
    | CLOSURE of inst list
    | LET
    | ENDLET
    (* function calls *)
    | APPLY
    | RETURN

    (* basic arithmetics*)
    | ADD
    | SUB
  and value =
    | VInt of int
    | VClos of inst list * value list
    | VEnv of value list
    | VInst of inst list

    (* debugging case *)
    | VUnknown of inst list * value list * inst list

  type environment = value list

  let interpreter code =
    let stk = [] in
    let (env: environment) = [] in
    let rec loop s e c = match s, e, c with
      | s, e, LDV n::cs -> loop (LDV n::s) e cs
      | LDV(VInt(a))::LDV(VInt(b))::s, e, ADD::cs ->
          loop (LDV(VInt(a + b))::s) e cs

      | LDV(VInt(a))::LDV(VInt(b))::s, e, SUB::cs ->
          loop (LDV(VInt(a - b))::s) e cs

      | s, e, ACCESS(n)::cs -> loop (LDV(List.nth e n)::s) e cs
      | (LDV v::s), e, LET::cs -> loop s (v::e) cs
      | s, (_::e), ENDLET::cs -> loop s e cs
      | s, e, CLOSURE c'::cs -> loop (LDV(VClos(c', e))::s) e cs

      | ((LDV v)::LDV(VClos(c', e'))::ss), e, (APPLY::cs) ->
          loop (LDV(VInst(cs))::(LDV(VEnv e))::ss) (v::e') c'

      | (v::(LDV(VInst(c)))::(LDV(VEnv e'))::ss), _, RETURN::_ ->
          loop (v::ss) e' c

      | (LDV(v)::_), _, [] -> v
      | s, e, c -> VUnknown (s, e, c)
    in loop stk env code
end

module TestSECDTailCalled = struct
  open SECDMachineNaive
  (* note: LVD(Vint 1) is loaded into the environment, to access we need to
     call ACCESS first. All operators operates on the stack.
   *)
  let t1 () =
    let open SECDMachineNaive in
    let p = [CLOSURE [ACCESS(0); LDV(VInt 2); ADD; RETURN]; LDV(VInt 1); APPLY]
    in interpreter p
end
