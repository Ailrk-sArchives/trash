⍝ multiline definition.
⍝ a dfn can have mutiple local assignment but only the last expression will be
⍝ evaluated and be used as the result.
mean←{
  sum←+/⍵
  num←⍴⍵
  ⎕←num
  sum÷num
}

⍝ Guards. a dfn can be used as a switch statement directly
sign←{
  ⍵>0: '+ve'
  ⍵=0: 'zero'
  '-ve'         ⍝ default case
}

⍝ shy result
⍝ if the function is side effect only you can discard the resutl.
log←{                   ⍝ append ⍵ to file alpha.
  tie←⍺ ⎕fstie 0        ⍝ tie number for file.
  cno←⍵ ⎕fappend tie    ⍝ new component num.
  tie←⎕funtie tie       ⍝ untie file.
  1:rslt←cno            ⍝ comp number as shy result. assign after a true guard.
}

⍝ static name scope
which←{
  type←'static'
  fn1←{
    type←'dynamic'
    fn2 ⍵
  }
  fn2←{ type ⍵ }
  fn1 ⍵
}

⍝ tail calls.
⍝ fact←{⍵<2: 1 ⋄ ⍵ × fact ⍵ - 1}

fact←{⍵≤1: 1 ⋄ ⍵ × ∇ ⍵ - 1}  ⍝ use recursion operator
fact¨⍳10

3 {(⍺×⍵) fact ⍵-1} 10   ⍝ this is a tail call.

⍝ recursion
pow←{
  ⍺=0:⍵  ⍝ a note on =. ≡ is a deep equality check for rank, depth, length too.
  (⍺-1)
}

⍝ apply ⍺ twice
⍝ (⍺⍺ ∘ ⍺⍺) ∘ (...) ... ⍵
exp←{ ⍺=0:⍺⍺ ⍵ ⋄ (⍺-1)⍺⍺∘⍺⍺ ∇∇ ⍵ }

succ←{1+⍵}          ⍝ successor
succ1←{1+⍺}          ⍝ successor

10 succ exp 0
