# Inductive definition

https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/02-inductive.pdf

To define all aspects of a programming language we need different  formalisms. E.g grammar rules for syntax, typing rules for static semantics, abstract machine for dynamic semantics.

If you look at how they're defined, their mechanism are all sorta the same: you have some axioms, some basic elements to work with, then a set of rules allows you to do natural deduction.

e.g1: typing rules for simply typed lambda calculus.
      Note this is general rules, to make it a real langauge we need to specify
      what specific types t can be.
```
   e:t ∈ Γ        Γ, e₁:t₁ ⊢ e₂:t₂         Γ ⊢ e₁:t₁→t₂, Γ ⊢ e₂:t₁
  -----------   -------------------     ----------------------------
    Γ ⊢ e:t       Γ ⊢ λe₁.e₂:t₁→t₂          Γ ⊢ e₁ e₂: t₂
```

e.g2: modern SECD machine. We deduce the next state of the machine from the
      previous state of the machine.
```
    ACCESS(n);c e s     CLOSURE(c');c e s       APPLY;c e v.c'[e'].s
  ------------------  ---------------------   ----------------------
      c e e(n).s         c e c'[e].s              c' v.e' c.e.s

    RETURN;c e v.c'.e'.s     LET;c e v.s     ENDLET;c e s
  -----------------------   --------------  ---------------
      c' e' v.s              c v.e s          c e s
```

e.g3: perfectly balanced bracket grammar. ∑ = { ( , ) }.
      notation: s M, where s is terminal and A is nonterminal. We say s is in
      syntax category M.
      Normally with bnf we write M ::= ε | (M) | M M
      We can transaliterate it into this, and saying the same thing:

```
            s M      s₁ M   s₂ M
  -----   --------  --------------
   ε M      (s) M      s₁ s₂ M
```

For all these cases we deduce a new case from a given case. In situations that no rules apply, we throw an error.
