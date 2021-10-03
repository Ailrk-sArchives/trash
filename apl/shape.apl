⍝ First we deal with scalars.
scalar1←1
scalar2←⊂1 2 3

⍬≡⍴scalar1  ⍝ scalars have zilde shape
⍬≡⍴scalar2

0 ⍬ 1 2 3≡⍴⍴scalar

⍝ vector
5=⍴'abcde'     ⍝ shape of 1 dim vector
1=⍴⍴'abcde'

⍝ Matrix
⍴'abc'∘.,1 2 3 4  ⍝ shape of 3 x 4 matrix

cuboid←'abc'∘.,1 2 3 4∘.×0J1 1J1  ⍝ two consecutive outer product gives a cuboid
3 4 2≡⍴cuboid
3=⍴⍴cuboid    ⍝ rank is 3

moredim←cuboid∘.,'ABC'  ⍝ now it's in higher dimension
4=⍴⍴moredim             ⍝ PS: = is for scalar equality, ≢for vector deep compare


⍝ first, reshape
r←3 3⍴⍳9∊1 2 3 4 7
⍝ monadic veritical rotate
⌽(3 3⍴⍳9)
⍝ dyadic vertical rotate. last rank rotate one to the right
1⌽(3 3⍴⍳9)
⍝ dyadic vertical rotate. first rank rotate one to the right
1⌽[0](3 3⍴⍳9)
⍝ dyadic vertical rotate. last rank rotate one to the left.
¯1⌽(3 3⍴⍳9)

R← ¯1⊖¯2⌽5 7↑r
C←1 0 ¯1∘.⊖1 0 ¯1 ⌽¨⊂R

R ∧ (5 7⍴⍳35)
display ≢⍴R
display (⍴⍴R)[0]
