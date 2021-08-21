(* Execution model of programs
   1. interpretation:
      - control expressed in host langauge.
      - langauge represented as tree shaped data type.
      - the interpreter traverse the tree during execution
   2. compile to native code:
      - control is compiled to machine code before execution
      - stand alone binary
   3.  compile to abstract machine code.
      - control compiled to some abstract instructions
      - instructions are designed closer to operations of the
        source langauge.

   In haskell ppl usually write dsl, but what is dsl really?
   One way to think about it is to defunctionalize some functions
   to get a representation of a program, then we can write a
   eval function base on the tree like data type. It's like the
   reverse direction of lisp macro, to manipulate elements of
   the target langauge we hoist it's abstract syntax as somethingo
   first class and we can work with. (being data type means we
   have full control over it).

   Little interpreter is everywhere. A common technique like
   defunctionalize the continuaition and build up monad operation    is actually making a interpreter. e.g ReadP parser combinator.

   Another way to run program is to define some middle level
   hypothetical machine instructions. These instructions are
   designed to be close to the langauge so the translation is
   easy. As long as we fully implemented the abstarct machine
   our languge meet the spec completely.
 *)

(*
 *)
module SECDMachine = struct

end
