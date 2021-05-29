{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Other.FixPoint where

-- y combinator.

{-@ Note how this definition looks different from ordinary Y combiantor
    which is \f -> (\x -> f $ x x) (\x -> f $ x x)

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

-- infinite
sinFix = fix (\f x -> if x == sin x then x else f (sin x))

-- oh
cosFix = fix (\f x -> if x == cos x then x else f (cos x))

-- with IO action.
printFix  = fix (\f x -> if x >= 0 then do print x; f (x - 1) else pure x) $ 4

many_ones = fix (1:)
onst_still_const = fix (const "hello")

v1 = fact 5

{-@  Curry's pardox: untyped lambda calclus is unsound as a deductive system, as
     the existence of Y combinator (fixed point combinator) makes it possible to
     have undecidable function.

     In a simply type lambda calculus, the existence of Y combinator force all types
     to be lifted. You need to have a bottom value beacuse you can't guarantee function
     terminates.
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
