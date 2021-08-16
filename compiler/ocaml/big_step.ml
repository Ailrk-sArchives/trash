(* big step semantics / natural semantics
   one step reduction is not very efficient, because each reduction
   step is not constant time.

   In small step reduction each reduction step we need to:
      find the next redex -> substitute -> reconstruct the term
   GOAL: We want to amortize the cost over whole reduction sequence.

    a b ->* (\x.c) b ->* (\x.c) v' ->* c[x <- v'] ->* v
       ^            ^
    a ->* \x.c    b ->* b'

    # Big step semantics:
    define relation a => v means a evaluates to value v.

    CBV big step semantics:
        N => N            \x.a => \x.a

      a => \x.c   b => v'  c[x <- v'] => v
     --------------------------------------
              a b => v
    CBN (value cases are the same as CBV)
      a => \x.c   c[x <- b] => v
     --------------------------------------
              a b => v


 *)

