⍝ Idiom list
⍝   Type     |    Rank
⍝  C char    |    S scalar
⍝  B bool    |    V vector
⍝  N num     |    M matrix
⍝  P nested  |    A array (tensor)
⍝  A any     |

)copy dfns display

⍝ -- rank:  ⍴⍴A    get dim of dim aka rank.
⍴⍴1 2 3         ⍝ => 1
⍴⍴2 2 ⍴ ⍳4      ⍝ => 2

⍝ -- sequence selection
0 1 0 1/⍳4

⍝ -- index selection

⍝ -- array selection
1 4 7 ⊃¨⊂ 3 3 3 4 4 4 5 5 5   ⍝ => 3 4 5
3 3 3 4 4 4 5 5 5[1 4 7]      ⍝ same

⍝ -- zero
{0}1 2 3

⍝ -- zero each
{0}¨1 2 3
{0}¨2 2⍴⍳4

⍝ -- join
⍝ ┌→────────────────────────┐
⍝ │ ┌→──────────┐     ┌→──┐ │
⍝ │ │     ┌→──┐ │ 4 5 │3 5│ │
⍝ │ │ 1 2 │3 4│ │     └~──┘ │  → all flat.
⍝ │ │     └~──┘ │           │
⍝ │ └∊──────────┘           │
⍝ └∊────────────────────────┘
display ↑↑,/¨,/(1 2 (3 4)) 4 5 (3 5)

⍝ -- enclose, first,  mix, split
⍝ enclose
⊂⊂⊂⊂⊂1 (1 2)        ⍝ box 5x

⍝ partitioned enclose (breaks at position with 1)
0 1 0 1 ⊂ ⍳4
⍝ first
'W'≡⊃ 'Word'

⍝ pick
'r'≡3 ⊃ 'Word'
⍝
↑ 'Hip' 'Hop'  ⍝ mix, get an matrix

⍝  ⍴->2                             ⍴->2 3      ⍴->2 3 2
⍝┌→──────────────────────┐       ↑              ↑↑
⍝│ ┌→──────────┐ ┌→────┐ │      ┌→──────────┐   ┌┌→──┐
⍝│ │     ┌→──┐ │ │2 3 4│ │      ↓     ┌→──┐ │   ↓↓1 0│
⍝│ │ 1 2 │3 4│ │ └~────┘ │      │ 1 2 │3 4│ │   ││2 0│
⍝│ │     └~──┘ │         │      │     └~──┘ │   ││3 4│
⍝│ └∊──────────┘         │      │           │   ││   │
⍝└∊──────────────────────┘      │ 2 3 4     │   ││2 0│
⍝                               │           │   ││3 0│
⍝                               └∊──────────┘   ││4 0│
⍝                                               └└~──┘
↑↑(1 2 (3 4)) (2 3 4)

⍝ -- join along the frist axis
((1 2 3) 3 4) 1 2 3 (1 )

⍝ only the frist axis is nested ⍝ so we flat everything
display ↑↑⍪/¨⍪/((1 2) 3 4) 2 3 (1 2)

⊃3 3⍴⍳10
⍝ -- upper right
⊃⌽3 3⍴⍳10
⍝ -- lower right
⊃⌽,3 3⍴⍳10

⍝ -- sink     A{}A   there will be no result back.
1 2 3{}5 6 7

⍝ -- left   A{⍺}A
1 2 3 {⍺} 'abc'    ⍝ => 1 2 3

⍝ -- right  A{⍵}A
1 2 3 {⍵} 'abc'   ⍝ => abc

⍝ -- link  A {⍺ ⍵} B
1 2 3{⍺ ⍵} 'abc'  ⍝ => 1 2 3 'abc'
