(* record and objects
   - type of an object is the set of messages it can send and receive.
   - we can describe it with recursive type and records.

   interface Point { pos : int; move int -> Point }
   typing : Point = μP.Rec{pos:int;move:int -> P}

   - note fixpoint μ is used to denote P itself as a recursive type

   Now we can Model oo subtyping with row poly:
     Rec{a : τ, b : σ, c : ρ} <:  Rec{a : τ, b : σ}

 *)

(* lose of information problem
      foo : Rec{a : Int} -> Int x Rec{ a : Int }
      foo = λx.(x.a, x)

      let (n, r) = foo({a:5, b:true})

    Note, type r "forget" the existence of b, information of b is lost.
 *)

module LossInfo = struct
  type record = {
    a: int;
    b: float;
    c: int
  }

  let r = { a=1; b=2.; c=3 }

  (*
     Assume we want to pass {a: int, b: float}, then the info of c: int is lost
     in r. We can't do that in ocaml, instead we must the same type.

     If we want subtyping polymorphism, we need to makesure the information
     perserve even if we pass a super type { a: int, b: float }. To achieve
     that we need row polymorpihsm
   *)
  let foo1 ({a: int; b: float; _} as r) = (a + Float.to_int b, r)
  let foo2 ({a: int; b: float; c: int} as r) = (a + Float.to_int b + c, r)

end

(* row poly can be used to address the loss of information.
     foo :: ∀r: row.(r/a) => Rec{a:int / r} -> int x Rec {a:int | r}
     foo = λx. (x.a, x)

    We think the current row always universally quantified by some extra rows.
    threse rows can be empty. This way the type is consistent, since we only
    added polymorphism.
 *)

(* Operations supported by row poly.

   Properties of record is the same, we add capability to add new fields.

   - field selection
      select: ∀α:type. ∀r:row(r/l) => Rec{l:α|r} -> α
    - field removal
      remove: ∀α:type. ∀r: row. (r/l) => Rec{l:α|r} -> Rec{r}
    - extension
      add: ∀α:type.∀r:row.(r/l) => α -> Rec{r} -> Rec{l:α|r}

    Notice an interesting property, names matters in type now.
 *)


(* row poly and type inference *)

module RowPoly = struct
  let o: <add : 'b -> 'a; length : int> as 'a = object
    val lst = []
    method add(x) = {< lst =(x :: lst) >}
    method length = List.length(lst)
  end

  let o2 = object(self)
    method happy = "Joy!"
    method print = Printf.printf "%s" self#happy
  end

  let p = object(self)
    method misery = "Woe!"
    method print = Printf.printf "%s" self#misery
  end

  (* we can do something like exitential wrapper *)

  (* upcast should be explicit
     And there is no downcast. Because downcasting requires runtime type
     information.
   *)
  type printable = <print:unit>
  let printable = [(o2 :> printable); (p :> printable)]

  let run1 = List.map (fun x -> x#print) printable

end
