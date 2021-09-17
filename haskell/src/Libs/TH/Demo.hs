{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Libs.TH.Demo where

-- some basic usage of template haskell
-- both c++ templates and lisp macros can be classified as metaprogramming
-- techniques, and more specially they are to achieve multi stage programming.

-- traditional compiled languages have two stages: compile time and runtime.
-- some information is strictly available at runtime, but not kown at compile
-- time.

-- template haskell add yet another layer on top, so now we have three stages
-- in compilation process: 1. code generation, compilation, and execution.

-- The code generation phase is rather like macro, but strictly written in
-- the same langauges, just like lisp macro. One can run arbitrary computation
-- at code genration step as if they are running another program.

-- C++ template also have this extra code generation phase, but the computation
-- is more strict: it uses a type level functional language the parameterized
-- on types. Thus, the template body acts like a quasi quoter, and tempalte
-- paramters are compile time known information to help generating the final
-- concrete code.

-- There are other methods to achieve meta programming. Namly reflection. It's
-- similar to tempalte haskell in the sense that we use the same langauge to
-- generate the code itself. The difference is that reflection relies on runtime
-- information, the code generator query information of the code.
--
-- template haskell happens before compilation, this is the same as c++
-- template and lisp macro. Lisp macros benefits from its homoiconicity, which
-- maans the list data structure a lisp macro manipulates is the same as the
-- lisp syntax itself, thus it greatly simlifies the code generation process.
--
-- C++ template on the other hand has simple mechanism, but whenever people
-- want to do advance code generations or meta programming they need to
-- hack around the system, thus make the meta programming style hard to read
-- and hard to reason.
--
-- Template haskell allows generate arbitrary code with haskell langauge itself,
-- it uses quasi quotation to easily create unparamteriezed ast, and allows
-- paramters to be passed in to create concrete code that can be compiled.

-- For a staged interpreter, there are some specs we care about:
-- 1. cross stage presistence
--    a varaible bound in one stage is accessible by following statges
-- 2. cross stage safty
--    a variable bound in one stage cannot be accessed by eariler stages.


