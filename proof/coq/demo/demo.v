(* Proofs and programs *)

Module Bool.

(** [Inductive] is Coq's way of defining algebraic datatype.  *)


Inductive bool : Type :=
  | true : bool
  | false : bool.


(** define ternary bool, where bool can be true, false, and unknown *)
Inductive tribool : Type :=
  | ttrue : tribool
  | tfalse : tribool
  | tunknown : tribool.


(** [Definition] Write function operate on bool by pattern matching *)
Definition negb (b:bool) : bool :=
  match b with
  | true => false
  | fasle => true
  end.


Definition andb (a b : bool) : bool := if a then b else false.

Definition orb (a b : bool) : bool :=
  if andb a (neg b) then true else
  if andb (neg a) b then true else false end.

(* simply enumerate all cases *)
Definition xorb (a b : bool) : bool :=
  match a with
  | true => match b with
            | true => false
            | false => true
  | false => match b with
             | true => true
             | false => false
             end.

(** New tactics
  [intros]: introduce variables into context.
  [simpl]:  simplify the goal.
  [reflexivity]: prove some expression [x] equals to itself.
*)

(** TODO *)

(** New tactics
  [destruct]: consider all possible constructors of an inductive data type,
  generataingsubgoals that need to be solved separately.
*)




