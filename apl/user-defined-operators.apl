⍝ user defined operators
⍝   it takes functions and produce a new function.
⍝   in the body you can use

⍝ TODO: now if ⍵ is > 1 the function will be applied multiple times.
if←{(⍺⍺⍣⍵)⍺}   ⍝ conditional function application.
2=1 {⍵+1} if 1  ⍝ 1 is applied
1=1 {⍵+1} if 0  ⍝ 1 is not applied
