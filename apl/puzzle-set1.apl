⍝ or just some examples

⍝ -- grille cypher
grid grille←5 5∘⍴¨ 'VRYIALCLQIFKNEVPLARKMPLFF' '⌺⌺⌺ ⌺ ⌺⌺⌺ ⌺ ⌺ ⌺⌺⌺ ⌺⌺⌺  ⌺⌺'
display grid grille

grid[⍸grille=' ']   ⍝ index with what is space in grille mask
(' '=,grille)/,grid   ⍝ ravel grids then match space

⍝ -- avg of list of numbers

{(+⌿⍵)÷≢⍵} ⍳100   ⍝ average
{⍵×(⍵+1)÷2} 100   ⍝ Sum(1 to 100)
{⍺, ',', ⍵}⌿'a' 'b' 'c'  ⍝ separate lists.
+/','≠'comma,delimited,text' ⍝ number of none comma chars.

⍸ 'mississippi' ∊'sp'   ⍝ indices of multiple elements
'mississippi'(⍸∊)'sp'

'abcd' ∘.= 'cabbage'    ⍝ frequency (occurrence) of char with outer product.

⍝ --  pascal triangle
⎕IO←1
pascal←{⍕0~¨⍨(-⌽A)⌽↑,/0,¨⍉A∘.!A←0,⍳⍵}
pascal 8

⍝ -- draw triangles.

⍝ idiom: NV⍴¨S, repeat S with each ele in NV.
{↑⍵⍴¨'⌺'} ⍳10

⍝  -- , smash an array
{(⍵,¨1)⍴¨'⌺'} ⍳10

⍝ longest common prefix
words ← 'flower' 'floor' 'flight'

⍝ use a train here.
⍝ ∧\ to avoid floating 1 like 1 1 0 0 1 0 0

lprefix1 ← {+/∧\(∧/⊃=⊢)¨↓⍉↑⍵}↑⊃

⍝ -- prime

prime←{G←{2=+⌿0=⍵∘.|⍵}⋄(G X)/X←⍳⍵}
prime 20

⍝ -- multiplication table
{⍵∘.×⍵}1↓⍳10

{a b←⍵⋄a×b}¨{⍵∘.,⍵}1↓⍳10    ⍝ first make a table then mulitply.
9 9 ⍴{a b←⍵⋄a×b}¨,{⍵∘.,⍵}1↓⍳10  ⍝ flat, apply, reshape

⍝ -- boolean array
~5<⍳10
1 0 1/1 2 3
1 2∊⍳10
2 4 5 7 8{(~⍵∊⍺)/⍵}⍳10

⍝ all numbers not in multiplication table are prime
z←1↓⍳1000 ⋄ z{(~⍺∊⍵)/⍺}z∘.×z

⍝ -- conway's game of life
)copy dfns display
⎕IO←0
⍝ first, reshape
3 3⍴⍳9
⍝ monadic veritical rotate
⌽(3 3⍴⍳9)
⍝ dyadic vertical rotate. last rank rotate one to the right
1⌽(3 3⍴⍳9)
⍝ dyadic vertical rotate. first rank rotate one to the right
1⌽[0](3 3⍴⍳9)
⍝ dyadic vertical rotate. last rank rotate one to the left.
¯1⌽(3 3⍴⍳9)
⍝ the original shape
r←(3 3 ⍴⍳9)∊1 2 3 4 7
⍝ center
R← ¯1⊖¯2⌽5 7↑r
⍝ creating it's 9 possible rotations.
⎕←'[1] R1 R2 R3: '
display R R R
⎕←'[2] horizontal rotate each on enclose'
display 1 0 ¯1⌽¨⊂R
⎕←'[3] vertical rotate each scale up'
C←1 0 ¯1∘.⊖1 0 ¯1 ⌽¨⊂R
display C
⍝ cell count
⎕←'neighbour counts of the original matrix R'
display +/+⌿C
⍝ rule: if neighboour count is 3 or 4 ...
display 3 4=+/+⌿C
⍝ rule: and the cell is occupied
display 1R∧3 4=+/+⌿C

R ∧ (5 7⍴⍳35)

display ≢⍴R
display (⍴⍴R)[0]
