⍝ APL index does not necessaily start from 0. we can set
⍝ -------------------------------------------------------------

⎕IO←0 ⍝ quad IO get 0

2*5 ⍝ * is for power
2×5 ⍝ this is for multiplication
6÷2 ⍝ division
6/2 ⍝ / is for over. this creates 2 2 2 2 2 2
¯3+3 ⍝ ¯ is for negation

⍝⍝ precedence
⍝ in APL there is no precedence, binop always assicates to
⍝ to the right.

⍝ We usually read apl code from right to left.
2×5+5

⍝ parenthesis still work
(2×5)+5

⍝⍝ assignment

a←(2×5)+5 ⍝ a gets 15
a           ⍝ evaluate a

⊢a←42 ⍝ a gets 32, and evaluate it right away
⊢b←(5×2)+5
⎕←a  ⍝ quad gets a. quad is bascially stdout

⎕←fiftySeven←57
⎕←five38←538
⎕←my_variable_name⎕←'hello world'

(a b _ d) ← 3 1 4 1   ⍝ unpack an array

⊢a←a+1,b←b+2

⍝ Valence (arity)

1 + 1 ⍝ dyadic +
!7    ⍝ monadic factorial
7×3 ⍝ dyadic +
×¯7 ⍝ monadic × for directoin

⎕←'  '

⍝ -------------------------------------------------------------

⍝ of the sum first array
⍝ ⎕ ← data ← (1 2 3 4) (5 2 3 8) (9 3 3 4) (5 3 9 4)
⎕ ← data ← (1 2 3 4) (2 5 8 6) (8 6 2 3) (8 7 6 1)

⊢data2d←↑data   ⍝ converts to 2d array.

+⌿data2d        ⍝ sum each column

⊃+⌿↑data        ⍝ pick the first value.


⍝ Shape of array.
⍝ a command to bring function into scope
)copy display

DISPLAY ⍴5
DISPLAY data
DISPLAY 'a' 'ABCD' ((1 2 3) (4 5 5)) (↑((1 2) (4 5) (2 3))) data2d

DISPLAY ↑(⍳3, ⍳3)

⎕←⍳10

⍬≡⍴5 ⍝ Does zilde match shape of 5?

m←↑(1 2 3 4)(5 6 7 8)(9 10 11 1 2)
DISPLAY m
⍴m

≢⍴m ⍝ get rank of m

⊃⍴m ⍝ get the frist element of dimension of m

⍝ apl functoins are rank polymorphic
⍝ operations scales to different ranks.

1 + 1
1 + 1 1 1
1 1 1 + 1 1 1

DISPLAY 2 4 ⍴ ⍳8
DISPLAY 2 4 ⍴ ⍳10   ⍝ simpy cut off the extra
DISPLAY 3 4 ⍴ ⍳10   ⍝ if the dimension exists the array, elements warps up.

⍴2 4⍴⍳10 ⍝ you can squeeze the code like this.


⍝ -- enclose and disclose
⍝ APL arrays are collection of scalars. But arbitrary value can be warpped as a
⍝ scalar.

⎕←v←1 2 3
⊂v ⍝ enclose the vector 1 2 3

DISPLAY ⊂v ⍝ draw like a box.

v≡⊂v

1≡≢⍴v ⍝ v has rank 1
1≡≢⍴⊂v ⍝ ⊂v is enclosed as a box, it's used as a scalar so rank 0.

v[1]

DISPLAY (1 ('hello' 2)) (3 ('world' 4) 5 ('!' (1 2 3) (4 5 6)))

⍝ -------- Indexing

⍝ -- bracket indexing []

⎕←v←9 2 6 4 5 8 7 4 0 1
v[0]
v[5 2]  ⍝ simply get elements at index 5 and 2.

v[3]←¯1 ⍝ mutation
v
⍝ --

DISPLAY m←3 3⍴4 1 6 5 2 9 7 8 3
DISPLAY m[1;1]
DISPLAY m[0;]       ⍝ row 1
DISPLAY m[1;]       ⍝ row 1
DISPLAY m[;1]       ⍝ column 1


