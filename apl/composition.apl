⍝ functions in apl

)copy dfns display

⍝ simple
{+1}∘{-2} 1

⍝ bind A∘fY this is to partially apply an diadic to make it monadic.
2 2∘⍴¨'AB'
sine←1∘○⋄sine 10 20 30
(*∘0.5)4 16 25
(2∘*)⍳8     ⍝ binary power

⍝ atop: (2 trans, left function is applied monadically to the right function)
-⍤÷ 4      ⍝ take reciprocol can negate
12 -⍤÷ 4   ⍝ perform right dyadic first then negate.
3 1 4 1 5~⍤∊1 2 3  ⍝ check member ship first then inverse boolean matrix

⍝ beside: {X}f∘gY   composition
rank←⍴∘⍴⋄rank¨'JIMMY' (2 3⍴⍳6)

+/∘⍳¨2 4 6
+∘÷/40⍴1    ⍝ Golden ratio

⍝ over: {X}f⍥gY, (gX) f (gY)
scores←82 90 76⋄weights←20 35 45⋄(weights×scores)÷⍥(+/)weights ⍝ Weight avg

a1←2 3,⍥⊂'text'⋄a2←2 3{⍺⍵}'text'⋄a3←(⊂2 3),⊂'text'⋄≡,/a1 a2 a3

⍝ fork: (3 train)

⍝ [Commute] f⍨ switch (flip essentially)
1 2 3 4 ⍴⍨ 2 2

⍝ [Constant] a⍨    const
'mu'⍨ 'any' ⎕NULL     ⍝ always returns its operand
1E100 ('mu'⍨) 1j1
¯1⍨¨⍳2 3              ⍝ (fmap . fmap) const (-1)

⍝ [Same] ⊣ ⊢ identify function
'L' ⊣ 'R'
⊣ 1 2 3

⍝ [Hydrant] ⍎ A   execute a piece of apl string.
⍎ '1+1'
⍎ '+/∘⍳¨2 4 6'
ab←'AB'⋄⍎ '2 2∘⍴¨ab'  ⍝ can splice easily to work as a quasi quote.

⍝ [Format] ⍕
6 2 ⍕ 3.125 0.002
