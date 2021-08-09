program demo;

//  always declare stuffs on the top.
const
  PI = 3.13;
  SS = 'String is single quotes';

type
  ch_array = array [0..255] of char;
  md_array = array of array of integer;

  // enum types
  months = (Jan, Feb, Mar, Apr, May,
            Jun, Jul, Aug, Sep, Oct,
            Nov, Dec);

  // sub range types.
  age: 18..100;

  // enum is full order.
  summer: May .. Sep

var
  // basic types
  int, c, d : integer;
  r : real;
  bool : boolean;
  ch : char;
  str : string;   // by default the length is 255
  s : string[50];
  mystr : ch_array;
  my2d : md_array;

  // additional types
  b : byte;
  shi : shortint;
  w: word;
  li : longint;
  lw : longword;

  rr : real; rs : single;
  rd : double;

begin
  int := 1;
  r := 3.14;
  ch := 'a';
  str := 'apple';
  bool := true;

  int := 1 + 1;
  int := int + 1;
  int := 4 div 3;

  bool := true or false and true xor true;
  ch := str[1];

  setlength(my2d 10, 10);
  for c := 0 to 9 do
    for d := 0 to 9 do
      my2d[c, d] := c * d;
end.

program fp;

var i, dummy : integer;

// wow.
// you declare first, then define the body in
// a block.
function fact(const a: integer) : integer;
begin
  if a >= 1 then
    fact := a * fact(a - 1);
  else fact := 1;
end; // terminate function with ;


procedure get_integer(var i : integer; dummy : integer);
begin
  write('Enter an integer: ');
  readln(i);
  dummy := 4;
end;

Begin
  (* Try some io *)
  dummy := 3;
  get_integer(i, dummy);
  writeln(i, '!= ', fact(i));
  writeln('dummy = ', dummy);
End;
