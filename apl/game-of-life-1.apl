⍝ A sliding window over the array. This works like a convolution map.

⎕IO←0
)copy dfns display

⍝ we find 1 in 3x3 matrix, and get an array (2 1) (2 2). Then we use ≢ to count
⍝ the resulting array, which gives 2.
2≡≢⍸(3 3⍴0 0 0 0 0 0 0 1 1)

⍝ this does the same things, but by summing the ravels.
2≡+/,(3 3⍴0 0 0 0 0 0 0 1 1)

⍝ ≢ (tally) being used monadically means getting the length of the array.
⍝ It's the same as this one:
⍝ [Zilde] ⍬: it's just an empty vector.
tally←{⍬⍴(⍴⍵),1}

⍝ an implementation for ⍸
where←{(,⍵)/,⍳⍴⍵}


⍝ Flat everything ,¨
(3+0,¨⊢) 1 1 2 1

0,¨(2 2⍴1 2 3 4)


⍝ a game of life step is just a state transition function.
⍝ A tacit function
⍝ Three parts
⍝   1. (≢⍸⍵)⌺ 3 3
⍝   2. ∊¨
⍝   3. 3+0,¨⊢

⍝ [Stencil] f ⌺ m : slide windwos of ⍴m and applies f to each window.
⍝ Edge will be padded 0

⍝ [Where] ⍸ a : for 1 0 0 1 1, it tells you 1 4 5 are 1
⍝ [Tally] ≢ a : counting vector length. (btw ≡ a gives depth)

gol←{≢⍸⍵}⌺3 3∊¨3+0,¨⊢ ⍝ shortest gol.

glider←3 3⍴0 0 1 1 0 1 0 1 1
dish←8 8⍴0
dish[(⊂3 3)+⍸glider]←1

⍝ full iteration
display 4⍴(gol⍣1⊢dish)(gol⍣2⊢dish)(gol⍣3⊢dish)(gol⍣4⊢dish)
