" Rules are simple.
" 1. everything is an object
" 2. functions are not called, messages are sent to objects
" 3. three types of mesesages: unary, binary, keyword.

"basics"

| x y |
x _ 4.
x := 5.
x := y := z := 6.
x := (y := 6) + 1.
"create a new object"
x := Object new.


"bool and arithmetics"
| b x y |
x := 1. y := 2.
b := (x = y).
b := b not.
b := (x > y).
b := 5 between: 3 and: 12.
b := 123 isKindOf: Number.

"if object 123 responds to message sqrt"
b := 123 respondsTo: sqrt.

b := x isNil.
b := $A isUppercase.

x := 6  + 4.
x := 1 + 2 * 3.
x := 5 sign.
x := 1.2 integerPart.


"blocks"
"blocks are objects and maybe assigned to a variable"
| x y z |
x := [ y := 1. z := 2. ]. x value.
x := [ :argOne :argTwo | argOne, ' and ' , argTwo.].
