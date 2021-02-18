program Ufind;

(*
 * Union find is useful for managing sets without intersections.
 *
 * Two primopes:
 * 1. union: merge two sets without intersection
 * 2. find: check if two elements are in the same set.
 *
 * Process:
 * All elements in a set forms a tree.
 * To check if two elements are in the same set, just check if
 * root nodes of the trees they are in are the same one.
 * At the beginning each element is a tree with one element,
 * the element itself is the root of the tree.
 *
 * You don't really use the tree directly, instead just making the connection so
 * you can find a determinsitic representation of the set.
 *)

(* All nodes are stored in the same array.
 * Each node has an unique id, the id is the same as it's index in the array.
 * The rootIdx is the parent idx of the element.
 *)
type
  Node = record
    rootIdx: 0..255;
    value: integer;
  end;
  PNode = ^Node;
  Pool = array [0..255] of Node;

  (* be aware that a root node can be the representation of the entire set. *)
  UnionFind = object
    pool : Pool;

    public
    constructor init;

    function get(i: integer): PNode;       (* get ptr to element from pool *)

    function find(n: PNode): PNode;        (* find the root node *)
    procedure union(n: PNode; m: PNode);   (* merge two sets *)
  end;


constructor UnionFind.init;
var
  i: integer;
begin
  for i := 0 to length(pool) do pool[i].rootIdx:= i;
end;

function UnionFind.get(i: integer): PNode;
begin
  get := @pool[i];
end;

(* rootIdx of root is always the same as itself *)
function UnionFind.find(n: PNode): PNode;
begin
  if pool[n^.rootIdx].rootIdx = n^.rootIdx then
    find := n
  else
    find := find(@pool[n^.rootIdx]);
end;

(* find(n)^.rootIdx always give the root itself *)
procedure UnionFind.union(n: PNode; m: PNode);
begin
  pool[find(m)^.rootIdx].rootIdx := find(n)^.rootIdx;
end;

(* Note, you cannot easily remove a node from the set unless
 * you make each link bidirectional. Because a parent don't really
 * know who is its child.
 *)

procedure Main;
var
  u: UnionFind;
begin
  u.init;
  writeln('node 1 is in set: ', u.find(u.get(1))^.rootIdx);
  writeln('node 2 is in set: ', u.find(u.get(2))^.rootIdx);
  writeln('node 3 is in set: ', u.find(u.get(3))^.rootIdx);

  writeln('now merge node 1 2 3');
  u.union(u.get(1), u.get(2));
  u.union(u.get(1), u.get(3));
  writeln('node 2 is in set: ', u.find(u.get(2))^.rootIdx);
  writeln('node 3 is in set: ', u.find(u.get(3))^.rootIdx);

  writeln('now merge node 5 7 8');
  u.union(u.get(5), u.get(7));
  u.union(u.get(7), u.get(8));
  writeln('node 5 is in set: ', u.find(u.get(5))^.rootIdx);
  writeln('node 7 is in set: ', u.find(u.get(7))^.rootIdx);
  writeln('node 8 is in set: ', u.find(u.get(8))^.rootIdx);

  writeln('is node 5 and node 8 in the same set? ',
    u.find(u.get(5))^.rootIdx = u.find(u.get(8))^.rootIdx);

  writeln('is node 2 and node 8 in the same set? ',
    u.find(u.get(2))^.rootIdx = u.find(u.get(8))^.rootIdx);

end;

begin
  Main;
 end.
