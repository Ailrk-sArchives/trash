signature BTREE =
sig
  datatype tree  = Leaf | Tree of tree * int * tree;
  val empty : tree
  val insert : (int * tree) -> tree
end

structure Btree : BTREE =
struct
  datatype tree  = Leaf | Tree of tree * int * tree;
  val empty = Leaf

  fun insert (key, Leaf) =  Tree (Leaf, key, Leaf)
    | insert (key, Tree (l, x, r)) =
    if key < x
    then insert (key, l)
    else if key > x
    then insert (key, r)
    else Tree(l, x, r)
end
