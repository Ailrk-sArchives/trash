⍝ game of life in apl.
⎕IO ← 0
r ← (3 3 ⍴ ⍳ 9) ∊ 1 2 3 4 7 ⍝ boolean matrix R.

⍝ 1. ↑ scale up r to 5x7 with.
⍝ 2. ⌽ rotate around vertical axes (bring elements 2 right)
⍝ 3. ⊖ rotate around horizontal axes  (bring elemnts 1 down)
R ← ¯1 ⊖ ¯2 ⌽ 5 7 ↑ r

)copy dfns display

display R R R

⍝ [Enclose]        ⊂n      to enclose R to form a rank 0 array.
⍝ [Each]           f¨Y     map monaic f to array Y.

⍝ [Inner produce]  f.g
ip1←1 2 3×.+4 5 6
ip2←3∧.=3 3 3 3
ip3←{⍵ +.× ⍵}2 2⍴1 2 3 4  ⍝ a matrix product

⍝ [Outer product] ∘.g
op1←1 2 3∘.×4 5 6 7

⍝ [Jot]           ∘         function composition
jot1←⌽∘⍳¨ 3 4 5    ⍝ iota map to array then reverse

display 1 0 ¯1 ∘.⊖ 0 ¯1 ⌽¨ ⊂R

⍝ gol2←{↑1⍵∨.∧3 4=+/,¯1 0 1∘.⊖¯1 0 1∘.⌽⊂⍵}

⍝ we can use tagged array to simulate data type.
mkTuple←{a b c ← ⍵ ⋄ 'tuple' a b c}
mkTuple 1 2 3

⍝ There is no sum type, we need to simulate branches like how we do in C.
