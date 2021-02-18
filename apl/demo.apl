w←⊃(⊃0⍴⍵){                           ⍝┌┌─2─┐       monadic; use ↓
    (e a)←|⍺                         ⍝├ 0 0 1 1 1  dyadic; use /
    T←⌽⍣(0>⊃⌽⍺)                      ⍝└──→⍺⍺←─────┐
    Pad←⍵⍵⍉(T⊣)⍪⍵⍪(T⊢)               ⍝ ┌⍺┐  ⌺     │
    need←(1+e),1↓⍴⍵                  ⍝ ┌─────⍵⍵──┐┘
    a=0:(1↓need⍴0↑⍵)Pad(1↓need⍴0↑⊢⍵) ⍝  0 0│1 2 3 4 5│0 0  Zero
    a=:(1↓need⍴1↑⍵)Pad(1↓need⍴1↑⊖⍵)  ⍝  1 1│1 2 3 4 5│5 5  Replicate
    a=2:(⊖¯1↓need⍴⊢⍵)Pad(¯1↓need⍴⊖⍵) ⍝  2 1│1 2 3 4 5│5 4  Reverse
    a=3:(⊖⊢1↓need⍴⊢⍵)Pad(⊢1↓need⍴⊖⍵) ⍝  3 2│1 2 3 4 5│4 3  Mirror
    a=4:(⊖¯1↓need⍴⊖⍵)Pad(¯1↓need⍴⊢⍵) ⍝  4 5│1 2 3 4 5│1 2  Wrap
}(¯1⌽⍳≢⍴⍵)/(⌽extra,¨⍺⊣0),⊂⍵          ⍝     └────⍵────┘
