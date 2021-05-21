program HelloWorld;
uses crt, SysUtils;

type
  Book = record
    title: string;
    author: string;

    reference: array[0 .. 1] of ^Book;  // have 2 references
  end;

  Position = record x, y: integer; end;

  // tagged union.
  // pascal has actual support, not like
  ShapeKind = (Square, Rect, Circ);
  Shape = record
    position: Position;
    case kind : ShapeKind of
    square : (side: integer);
    rect : (length, height : integer);
    circ : (radius: integer);
  end;

  ShapePtr = ^Shape;

  // object on stack by default, class on heap (its just a polymorphic ptr).
  // the only purpose of this class is to move by (dx, dy)
  ShapeMover = object
    private
    shape: ^Shape;

    public
    constructor init(s: ShapePtr);
    procedure move(dx: integer; dy: integer);
  end;

constructor ShapeMover.init(s: ShapePtr);
begin
  shape := s;
end;

procedure ShapeMover.move(dx, dy: integer);
begin
  shape^.position.x := shape^.position.x + dx;
  shape^.position.y := shape^.position.y + dy;
end;

function factorial(n: integer): integer;
begin
  if n < 2 then
    factorial := 1
  else
    factorial := factorial(n - 1) + factorial(n - 2);
end;

procedure sum3Factorials;
var
  a: integer;
  b: integer;
  c: integer;
  fsum: integer;
  fsumPtr: ^integer;  // pointer
begin

  writeln('Hello world');
  writeln('input three values: ');
  readln(a, b, c);
  fsumPtr := @fsum;
  fsumPtr^ := factorial(a) + factorial(b) + factorial(c);

  writeln('the sum of factorials is: ', fsum);
end;

procedure mkBookList;
var
  bookLists: array [0..3] of Book;
  bookPtr: ^Book;
  refLen: integer;
  bookLen: integer;
  k : integer;
  n, m : integer;

begin
  bookLen := length(bookLists);
  bookPtr := nil;

  refLen := length(bookLists[0].reference);

  for n := 0 to bookLen do
  begin
    bookLists[n].title := 'sf' + IntToStr(n);
    bookLists[n].author := 'author' + IntToStr(n);

    for m := 0 to refLen do
    begin
      k := (bookLen + m) mod bookLen;
      bookLists[n].reference[m] := @bookLists[k];
      k := k + 1;
    end;
  end;

  // print books
  for n := 0 to bookLen do
  begin
    writeln('book: ', bookLists[n].title);
    writeln('author: ', bookLists[n].author);
    writeln('ref:');
    for m := 0 to refLen - 1 do
    begin
      bookPtr := bookLists[n].reference[m];
      if bookPtr = nil then
      begin
        writeln('no ref');
        break;
      end
      else writeln('   - ', (bookLists[n].reference[m]^).title);
    end;
  end;
end;

procedure oopWithObject;
var
  s1: Shape;
  smover: ShapeMover;
begin
  s1.kind := Circ;
  s1.radius := 10;
  writeln('old position: ', s1.position.x, ',', s1.position.y);
  s1.position.x := 0;
  s1.position.y := 0;
  smover.init(@s1);
  smover.move(1, 1);
  writeln('new position: ', s1.position.x, ',', s1.position.y);
  smover.move(3, 1);
  writeln('new position: ', s1.position.x, ',', s1.position.y);
  smover.move(-10, 10);
  writeln('new position: ', s1.position.x, ',', s1.position.y);
end;

begin
  writeln('sum 3 factorials');
  sum3Factorials;

  writeln('make a book list');
  mkBookList;

  writeln('oop with object');
  oopWithObject;
end.
