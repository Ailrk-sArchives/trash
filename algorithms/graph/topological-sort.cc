#include <iostream>
// template topological sort
// get a total ordered list (topological order) from a graph without cycle (a
// DAG). note a DAG might have multiple topological ordering.
//
// A topological orerding can be useful for scheduling dependencies. So notable
// examples: dependeneis management of makefiles, resolving symbols in
// linkers...
//
// Given a graph:
// 5 -> 11, 7 -> 11, 7 -> 8
// 3 -> 8, 3 -> 10
// 11 -> 2, 11 -> 9, 11 -> 10, 8 -> 9
//
// Possible topological order:
//  5 7 3 11 8 2 9 10
//  3 5 7 8 11 2 9 10
//  ...
//  All verteices are reached after their dependencies are reached.
//
// Typical topo sort running type O(|V| + |E|).
//
// Two algorithms:
//    1. Kahn's algorithm
//    2. DFS
//

// Some list utility

template <typename... Links> struct Graph {};

#define graph(...)                                                             \
  Graph<__VA_ARGS__> {}

#define node(node_) auto (*)(NodeWrapper<node_>)

template <typename N> struct NodeWrapper {
  void operator()() { return N(); }
};

template <typename> struct Edge {};
template <typename Node1, typename Node2>
struct Edge<auto (*)(Node1)->auto (*)(Node2)> {
  using From = Node1;
  using To = Node2;
};

// simulate GHC pipeline
//      Say we have this src
//
//  Parsec.hs -+
//  AST.hs    -+-> Lexer.hs  --- Parser.hs --+
//                                           |
//  Vector.hs -+                             |
//  LLVM.hs   -*-> Codegen.hs ---------------+---> obj.
//
//  Compilation itself has a topological order, on top of that
//  you have extra orders for separation compilation and linking.
void Source() { puts("Source"); }
void Parse() { puts("Parsing"); }
void Rename() { puts("Renaming"); }
void Typechek() { puts("TypeChecking"); }
void Desugar() { puts("Desugaring"); }
void Simplify() { puts("Simplifying"); }
void CoreTidy() { puts("CoreTidying"); }
void CorePrep() { puts("CorePrepring"); }
void ConvertToIfaceSyn() { puts("ConvertingToIFaceSyn"); }
void Serialize() { puts("Serializing"); }
void HIfile() { puts("HiFile"); }
void ToSTG() { puts("ToSTG"); }
void CodeGenCmm() { puts("CodeGenCmm"); }
void ToLLVM() { puts("ToLLVM"); }
void LinkLLVM() { puts("LinkLLVM"); }
void Exeutable() { puts("Executable"); }
