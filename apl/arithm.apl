⍝ monadic arithm

⍝ × gives signum
×1 2 ¯1 ¯3 0 1 2

⍝ ÷ gives reciprocol
÷3 2

⍝ e to the power of
*2 3 5

⍝ + negates imaginary part.
+0J1 0J¯1

⍝ ⍟ natural log of
⍟3 4 5

⍝ b⍟V log of
10⍟3 4 5

⍝ Matrix multiplication
(1+2 2⍴⍳4)+.×4+2 2⍴⍳4
(1+2 2⍴⍳4)+.×4+2 1⍴⍳2   ⍝ 2x2 . 2x1

⍝ ⌹ monadic gives you the inverse
⌹2 2⍴⍳4

X←(1+2 2⍴⍳4)⋄Y←(4+2 2⍴⍳4)⋄(Y+.×X⌹Y)≡X

⍝ ⌹ solve a system of linear equation
(⌹4+2 2⍴⍳4)+.×1+2 2⍴⍳4

⍝ ⌹ matrix division.
(1+2 2⍴⍳4)⌹4+2 2⍴⍳4

⍝ ! fact
!10

⍝ k!n binomial. Note the order is choose k from n
2!5

⍝ a?b generate a random num within b range
3 ? 5

⍝ | magnitude
| ¯1

⍝ | divide, return remainder
2 | 5

5 |⍨ 2  ⍝ in mod order

⍝ ceiling
⌈10÷3

⍝ dyadic max
3⌈5

⍝ floor
⌊10÷3

⍝ dyadic min
3⌊5

⍝ n⊥V evaluate V as base n
⍞←2⊥1 0 1 0 1 0

⍝ ⊤ break a number down into mixed base.
⍝ 10000 seconds is 2 hour, 60 min, 60 sec.
⍞←2 60 60⊤10000

⍝ convert from decimal to binary
⍞←(8⍴2)⊤42

⍝ depth (amount of nesting)
⍞←≡(1 2 (3 4 5 (6 7 8)))

⍝ ≢B length (how many major cells B has)
⍞←≢(1 2 (3 4 5 (6 7 8)))

⍝ ∨ for boolean array
1 0 1 0∨0 1 0 1

⍝ ∨ gcd for non 0 1
3∨9=3

⍝ ∧ lcm
3∧10=30

⍝ take
2↑3 3⍴'abcdefghi'
2 2↑3 3⍴'abcdefghi'
2↑[1]3 3⍴'abcdefghi'

⍝ take will pad
5↑3 3⍴1
5↑[1]3 3⍴1


⍝ monadic ↑ exchanges one level of depth into one level of rank
↑(1 2 3)(4 5 6)

⍝ missing element will be padded
↑(1 2 3)(4 5)

⍝ ↓ drops instead

⍝ monadic lower the rank and increasing the depth
↓3 3⍴1

⍝ indexing
1 2⌷2 3⍴⍳6

(2 3⍴⍳6)[1; 2]

(⍳10)[1 4 5]

A←3 3⍴'abcdefghi'⋄B←1 2⌷2 3⍴⍳6(3 4⍴)
