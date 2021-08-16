(* a simple language with sos style reduction
   Asuming there is no free variables.

   # Call by value in structural operational style (SOS)

   Term a, b := N
              | x
              | \x.a
              | a b
   Values v ::= N | \x.a

   - one step reduction relation a -> a': -------------------------------------

      (\x.a) v -> a[x <- v]  (Betav)

        a -> a'                     b -> b'
     -------------(App-l)         -----------(App-r)
      a b -> a' b                  v b -> v b'

    - properties --------------------------------------------------------------
      1. it's a weak redution, we cannot reduce under lambda abstraction.
          e.g  no this rule
          a -> a'
        ------------X
        \x.a -> \x.a'

      2. call by value, for a redex (\x.a) b, b must be fully reduced to a value
         before beta reducition
         (\x.a) v -> a[x <- v]
         (\x.a)(b c) -> a[x <- b c] --X

      3. left to right application. In application a b, we need to reduce a
         to a value firsts before we can reduce b.
          b -> b'
        -------------
         v b -> v b'

      4.  deterministic. forall a, there is at most one a' such that a -> a'

    - Reduction sequences -----------------------------------------------------
      - Termination: a -> a1 -> ... -> v
      - Divergence: a -> a1 -> ... -> ...
      - Error: a -> a1 -> ... -/->

    - e.g
      Termination:
         (\x.\y.y x)((\x.x) 1)(\x.x)
      -> (\x.\y.y x) 1 (\x.x)   -- App-l, App-r, Betav
      -> (\y.y 1) (\x.x)        -- App-l, App-r, Betav
      -> (\x.x) 1               -- Betav
      -> 1                      -- Betav
      Error:
         (\x. x x) 2 -> 2 2 -/->
      Divergence (omega):
         (\x.x x)(\x. x x) -> (\x. x x)(\x. x x) -> ...

    - PS: also there is `Reduction context` which bascially doing the same thing

   # Call by name in SOS style

      (\x.a) b -> a[x <- b] (Betan)

        a -> a'
      ------------ (app-l)
      a b -> a' b

   - property -----------------------------------------------------------------
     1. does not evaluate argument before application
     2. beta reduction is performed as soon as possible.
     3. argument is evaluated when it's used.

   # CBV vs CBN
     1. M terminates in CBV -> M terminates in CBN
     2. some terms terminates in CBN doens't terminate in CBV
        e.g (\x.1) omega
              where omega = (\x.x x)(\x.x x)
        in CBV, omega will not terminate so (\x.1) reduction will never perform
        in CBN, substitution happens first, so we get 1 right away.
     3. We can encode CBN in CBV languages with thunks, but the reverse requires
        CPS transformation.

 *)

type term = Const of int
          | Var of string
          | Lam of string * term
          | App of term * term

let isvalue = function
  | Const _ -> true
  | Lam _ -> true
  | _ -> false

let rec subst x v = function
  | Const n -> Const n
  | Var y -> if x = y then v else Var y
  | Lam(y, b) -> if x = y then Lam(y, b) else Lam(y, subst x v b)
  | App(b, c) -> App(subst x v b, subst x v c)


(* Natural semanticse (Big step semantics)
 *)

(* record types *)
type ratio = {
  num: int;
  demon: int;
};;

let add_ratio r1 r2 = {
  num = r1.num * r2.demon + r2.num * r1.demon;
  demon = r1.demon * r2.demon;
};;

let integer_part r =
  match r with
    { num=num; demon=demon} -> num / demon;;

type number = Int of int | Float of float | Error
type sign = Positive | Negative


type 'a btree = Empty | Node of 'a * 'a btree * 'a
