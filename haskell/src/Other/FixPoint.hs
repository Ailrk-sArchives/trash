{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Other.FixPoint where
-- https://matt.might.net/articles/implementation-of-recursive-fixed-point-y-combinator-in-javascript-for-memoization/
-- y combinator.

-- It's good to think Y as a searching algorithm, looking for the fix point
-- of it's argument -- a functional, and the fix point will be of the type
-- that the functioanl return.

-- What is a fixed point?
--   A fixed point is a value that is mapped to itself by a function
--   It comes with a function, as we say we find fixed points of a function f.
--   In algebra fix point just means a value x such that
--     x = f(x).
--   If we have f(x) = x * 2 - 2, the value make x = f(x) is 2, then 2 is
--   the fix point.
--   Thus in this case fix point is bascially the solution of the function.
-- What is functional?
--   Functional is a combinatory logic concept. It's bascially higher order
--   function that takes a function as it's input.
-- What is Y combinator?
--   Given a functional F, what's it's fix point? does there exists a x that
--   x = F(x)?
--   First, realize x should be a function because F is a functional.
--   Assume we have this function Y that help us to find such fix point.
--   so Y(F) find the fix point of F, and it's itself a function.
--   How should such a function be defined?
--    Y(F) = F(Y(F))
--   Y(F) = x, which is the unique value that makes x = F(x)!
--
-- Some tricks to avoid explicit recursion:
-- Y = \f. (\x. f (x x)) (\x. f (x x))
--
-- Evaluation:
--   Y g  -> ((\x. f (x x)) (\x. f (x x))) g // assume normal order
--       -> (\x. g (x x)) (\x. g (x x))      // applicative order y combinator
--       -> g ((\x. g (x x)) (\x. g (x x)))  // is defined a bit different.
--       -> g (Y g)
--       -> g (g (Y g))
--       -> ...
--
-- From another point of view, how does it recurse?
--   we're referring to the same thing through the same name twice. the body
--   of (\x . f (x x)) is copied twice everything we apply.
--
-- What's the matter?
--   To recurse we normally do recursion or iteration, but fix point give us
--   another way to think about these non trivial functions.
--   e.g use variations of Y combinators Ymem for memoization

{-@ Note how this definition looks different from ordinary Y combiantor
    which
    Y = \f. (\x. f (x x)) (\x. f (x x))

    It's done with lazy eval.
    x is bind to the result of (f x), which then get evaluate
    so let x = f x
        in f (f x)  -- (f x) is x, the x in (f x) is (f x), oh yeah
        in f (f (f x))
        ...
@-}

-- fix f = f (f (f ...))
fix f =
  let x = f x
   in x

fact = fix f
  where f rec n | n == 0 = 1 | otherwise = n * rec (n -1)
v1 = fact 5

-- infinite
sinFix = fix (\f x -> if x == sin x then x else f (sin x))

-- oh
cosFix = fix (\f x -> if x == cos x then x else f (cos x))

-- with IO action.
printFix  = fix (\f x -> if x >= 0 then do print x; f (x - 1) else pure x) $ 4

many_ones = fix (1:)
onst_still_const = fix (const "hello")

fib = fix
    $ \f ->
      \n -> if n == 0 then 0 else if n == 1 then 1 else f (n -  1) + f (n - 2)


{-@  Curry's pardox: untyped lambda calclus is unsound as a deductive system because
     of the existence of Y combinator (fixed point combinator) makes it possible to
     have undecidable function.

     In a simply type lambda calculus, Y combinator force all types to be lifted.
     You need to have a bottom value beacuse you can't guarantee function
     terminates.

     Note Y g might terminate, or might not. It depends on what g is. it's just
     that it's possible for a function to have a unhalting fixpoint, thus
     never give us an answer.
@-}

-- fix point data type. You can essentially do all recursion with this.
-- (except irregular stuffs like an AST. You might use )
data Fix f = In (f (Fix f))

{-@ Reistricted version of Fix:
    Mu and Nu,
    inductive finite data and coinductive infinite data
@-}

newtype Mu f = Mu (forall a . (f a -> a) -> a)
data Nu f = forall a . Nu a (a -> f a)

-- more of these topic on free algebra.

{-@ Mu for inductive finite data.
    Nu coinductive infinite data (a stream)
@-}

newtype Stream a = Stream (Nu ((,) a))
newtype Void a = Void (Mu ((,) a))
