{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}


-- paper on haskell inliner 20 years ago..
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf
module GHCs.InlineAndSpecialization where

-- INLINEABLE pragma:
--    1. causes the function be included into the
--       interface file
--    2. the function will be specialized at call sites.
--       even cross modules.

bar :: Int -> Int -> Int
bar a b = a + b
{-# INLINEABLE bar #-} -- this will appear in interface file.

-- INLINE
--    1. like INLINABLE, function is included in the
--       interface file.
--    2. GHC will be very keen to inline these functions
--    3. bloat compile time. compile each inlined call site
--       once.
--    4. inlining a function that doesn't trigger any optimization
--       further will have no benefits. Rather than saving a function
--       call.
--    5. If a function is inlined, and within the context it can trigger
--       more compiler optimization than being stand alone, it will be golden.
--    6. use INLINE to force RULES to fire.
bar' :: Int -> Int -> Int
bar' a b = a + b
{-# INLINE bar' #-} -- this will appear in interface file and very likely to be
-- inlined

bar'' :: Int -> Int
bar'' a = a + 1 -- this will automatically inlined no doubt. And it will be
-- an optmized unfolding.

-- Unfolding
--    unfolding of a function f is what f is replaced by after it is inlined.
--    usually the definiton of f. But it can also be a rewrite rule.
--
--    when will unfolding be added to interface files?
--    1. a function is small and GHC see it fits.
--    2. INLINE and INLINABLE
--    3. -fexpose-all-unfoldings

-- Optmized and unoptimized unfolding
--    GHC will include some small unfolding in interface files.
--    those unfolding (definitions) are optimized first to avoid repeatedly optmize
--    the same code.
--
--    NOTE: unfolding included by INLINABLE and INLINE is unoptimized so they
--    works better with RULES. That's why inlined function compiles so slow.
--    You are forced to recompile again.

-- How does Typeclass constraints get implemented?
-- Given this haskell function.
-- foo :: Show a => a -> a -> Bool
-- foo x y = show x == show y
--
-- GHC generates the core as:
-- foo = \ @a $dShow x y -> eqString (show $Show x) (show $Show y)
-- where @a is a type, d$Show is a dictionary with Show method of type @a.
--
-- So at run time, the typeclass constraints will actually be a extra dictionary with
-- methods in it get passed as an argument.

data Show' a = Show' {show' :: a -> String}

data Foo
  = Foo
  | Bar

-- this will be passed at runtime
showa :: Show' Foo
showa =
  Show'
    { show' = \case
        Foo -> "<Foo>"
        Bar -> "<Bar>"
    }

-- at runtime if we see a is Foo it's specialized like this.
foo' :: Show' Foo -> Foo -> String -> String
foo' showa foo xs = (show' showa foo) ++ xs

-- in normal haskell
foo :: Show a => a -> String -> String
foo a xs = show a ++ xs

-- Specialization
-- again,
-- foo :: Show a => a -> a -> Bool
-- if we know what a is, we can specialize a ahead of time, which generates much better code.
-- much better in the sense that you no longer need to pass a dictionary at runtime, but rather
-- hard code the specialized type.

-- When does specialization occurs?
--    1. INLINABLE or INLINE
--    2. -fspecialise-aggressively.
--    Bascially for most of the time it doesn't occur.
--    But if you use inline all the time it occurs everywhere.
--    There is a trade off you need to balance.

-- for foo, if we can specialize type a to be Foo at compile time, the further optimization
-- can directly insert the body of Show' Foo into foo at compile time.

-- SPECIALIZE works like template full specialization. You replace the polymorphic parameter
--            with a concrete type, and let the further optimization pass to handle it.

deriving instance Show Foo

foo''' :: Show a => a -> String -> String
foo''' a xs = show a ++ xs
{-# SPECIALIZE foo''' :: Foo -> String -> String #-} -- here foo''' get specialized.

-- think about code transformation.

-- inlining can cause code bloat?
--   1. same function body every where, of couse code bloat.
--   2. but inlining can reduce code size if it trigger some optization.

