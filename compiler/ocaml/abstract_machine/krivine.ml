(* Krivine's machine implements call by name semantics for lambda calculus.
   It also has three stacks: S, E, C
   for SECD, S, E holds value. But for krivine machine, S, E always hold thunk
   e.g a c[e]
   thunks won't evauluate until they are used.

   SECD machine follows apply eval model, and krivine machine uses push enter
   model.

   Call by name application forms a tree of spines: a node represents the
   application that uses an closure and an argument.
          @
        /   \
       @     a2[e2]     The stack encodes the spine of applications
     /   \              env and code encodes terms at bottom left of each
   n[e]  a1[e1]         spine.
   ^
  ---    -----------
  code      stack

  ACCESS(n)
          @
        /   \
       @     a2[e2]
     /   \
(\a)[e']  a1[e1]

 GRAB
           @
      /         \
 a[a1[e1].e']   a2[e2]

  Krivine machine only suuports call by name semantics, but a realistic call
  by name language needs to have at least two more features to be efficent:
    1. Strict operations on primitives.
    2. Sharing with lazy evalution.
 *)

module KrivineMachine = struct
  exception Error

  (* Note the debrujin index starts from 0 *)
  type inst =
    | LDV of value

    (* access has different semantic as that of CBV. ACCESS(n) means
       start evaluating nth thunk on the stack.
     *)
    | ACCESS of int
    | GRAB
    | PUSH of inst list

    | CLOSURE of inst list
    | LET
    | ENDLET

    (* function calls *)
    | APPLY
    | TAILAPPLY (* case for handling extra return frame *)
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
    | VUnknown of value list * value list * inst list

  type environment = value list

  let interpreter code =
    let stk = [] in
    let (env: environment) = [] in
    let rec loop s e c = match s, e, c with
      | s, e, LDV n::cs -> loop (n::s) e cs

      (* access evaluates the nth thunk on the stack *)
      | s, e, ACCESS(n)::_ -> begin match List.nth e n with
                              | VClos(c', e') -> loop s e' c'
                              | _ -> raise Error
                              end

      (* grab a new argument to the environment *)
      | VClos(c', e')::ss, e, GRAB::cs -> loop ss (VClos(c', e')::e) cs

      (* push a thunk from code c *)
      | s, e, PUSH c'::cs -> loop (VClos(c', e)::s) e cs

      | s, e, c -> VUnknown (s, e, c)
    in loop stk env code
end
