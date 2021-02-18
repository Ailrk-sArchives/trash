program Ptrs;

type
  Node = record
    data: integer;
  end;
  PNode = ^Node;  (* well, you always alias your pointer type..*)

var
  n: Node;
  nodePtr: PNode;

function foo(p: PNode): PNode;
begin
  p^.data := 10;
  foo := p;
end;

begin
  n.data := 0;
  writeln('before: ', n.data);
  nodePtr := @n;
  foo(nodePtr);
  writeln('after: ', n.data);
end.
