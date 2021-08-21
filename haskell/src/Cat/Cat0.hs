module Cat.Cat0 where

-- being a unifying descrptional tool, category needs to be simple enough to
-- encompass different situions. The core concept of a category is very simple:
--    Category C has
--      object:     a set Ob such that x in Ob are objects of C,
--      morphisms:  for every X, Y in Ob(C), exists Hom(X, Y) from X to Y in C
--                  if f is a morphism from X to Y it's written as f: X -> Y
--      identity morphism: For all X in Ob(C) there exists an identity morphism
--                  Hom(X, X)
--      composition: For X, Y, Z in Ob(C), Hom(X, Y) x Hom(Y, Z) -> Hom(X, Z)
--                   for f: X -> Y, g: Y -> Z, we have (g o f): X -> Z
--      axioms:
--        associativity: f: X -> Y, g: Y -> Z, h: Z -> W,
--                       f o (g o h) = (f o g) o h
--        identity: f:X -> Y => idY o f = f o idX

-- from simple arithmetics on real number to groups rings fields moduels to
-- category theory, learning algebra means study the rules and examples.
--
-- Some exmaple categories:
--   1. Set, Ob(Set) is a set of all set, morpihsms are function between sets.
--        e.g sin and cos are two morphisms from R to [1, -1], they
--            are two different morphisms for the same src and target.
--   2. Grp, Ob(Grp) is a set of all group, morphisms are grp homomorphisms.
--   3. partial order (P, <=), Obj((P, <=)) is elements in P, and there
--         exists a morphism between two objects A, B if A <= B.
--         note there can be multiple morphisms with the same source and target
--         the category looks like a lattice.

-- Hask category is actually not very interesting because it's only one
-- category. The power of category is the relationship between cats with
-- functor and natural transformations.


