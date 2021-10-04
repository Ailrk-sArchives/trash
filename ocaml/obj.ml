class ['a] relay = object(self)
  val mutable l : 'a list = []
  method add x = if not (List.mem x l) then l <- x::l
  method remove x = l <- List.filter(fun y -> x <> y) l
  method send m = List.iter m l
  method copy = Oo.copy self
end


class amateur = object (self)
  method play x risk = if Random.int risk > 0 then x else self
end

(* a bit like dependent type *)
class professional k = object (self)
  inherit amateur as super
  method level = k
  method! play x risk = super#play x (risk + self#level - x#level)
end

let pro1 = new professional 1


(* formalize the object layer
   - Object types are structural
   - class type are polymorphic in the type of self allow further refinement.
 *)




