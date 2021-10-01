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

⍝ pascal triangle
⎕IO←1
pascal←{⍕0~¨⍨(-⌽A)⌽↑,/0,¨⍉A∘.!A←0,⍳⍵}
pascal 8

