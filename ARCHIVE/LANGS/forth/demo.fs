\ normal commnet
( comment when defining words )

\ just load some numbers to the stack. 65 is at the top now
1 2 3 4 5

\ dump stack contains. this will not consume anything
.s

\ pop and print stack top
.
\ again
.
\ now we have 2 elements less
.s

\ 9 pushed, then popped immediately for .
5 4 + 6 * 5 mod 100 - abs .


\ --- stack manipulation.
\   When thinking about stack you really should think about passing parameters
\   that suddenly makes some wierd oprations more sensible.

3 dup - .     \ duplicate top items
2 5 swap / .  \ swap top two

1 2 3 rot .s  \  rotate the top 3 elements

4 0 drop 2 /  \ drop (pop without doing anything)

1 2 3 nip .s  \ remove the second elemnet

1 2 3 4 tuck  \ dup the top item below the second
1 2 3 4 over  \ dup the second item to the top
1 2 3 4 2 roll  \ move item at the position (on stack top) to the top
1 2 3 4 2 pick  \ dup the item at the position to the top

\ defining words

: square ( n -- n ) dup * ;
5 square .

42 42 = .

12 53 = .

\ the point of dup is to copy the top then use it for the next word witout
\ losing the value.

: ?>64 ( n -- n ) dup 64 > if ." Greater than 64!" then ;

: ?>65 ( n -- n ) dup 65 > if ." Greater than 65!" else ." Less than 65" then ;

65 ?>64 drop .s

65 ?>65 drop .s


\ --- loop

: myloop ( -- ) 5 0 do cr ." Hello!" loop ;

: one-to-12 12 0 do i . loop ;

one-to-12

\ n to 0. if n == 0 then skip
: squares ( n -- ) 0 ?do i square . loop ;

10 squares

\ +loop for steps
: threes ( n n -- ) ?do i . 3 +loop ;

15 0 threes

: death ( -- ) begin ." keep going..." cr 0 until ;

variable age

21 age !

age @ .

age ?

100 constant WATER-BOILING-POINT
WATER-BOILING-POINT .

\ --- arrays

variable mynums 2 cells allot  \ array with 3 cells

mynums 3 cells erase
mynums 3 cells 0 fill


\ short hand
create mynumbers1 65 , 011 , 1337 ,


\ same as as
53 mynums 0 cells + !
53 mynums 1 cells + !
53 mynums 2 cells + !


0 cells mynums + ?
1 cells mynums + ?

: of-arr ( n n -- n ) cells + ;

mynums 2 of-arr ?

200 mynums 2 of-arr !


\ -- return stacks

\ i duplidates the top of the return stack. Same as r@
: myloop1 ( -- ) 5 0 do r@ . loop ;

\ push 7 to return stack then swap 6 and 4.
5 6 4 7 >r swap r> .s


\ -- floating points

8.3e 0.8e f+ f.
