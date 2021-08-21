{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
module Cat.Cat.Monads where

{-@ 2020-04-30
    Functor: morphism between categories
    Endofunctor: A functor from a category to itself.
    Natural transformation: 2-morphism between two functors.
@-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans


-- Simple monad transformer.
-- 1. Monad transformers are themselves, monad transformers
-- 2. Monad transformers works sort like like a list. m in m (Maybe a)
--    is a monad, which of course can itself be another monad t ransformer.
-- 3. Stacking monad transformers allows you combine possible actions.
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}


-- what does it means to be a monad?
-- 1. it's are also functor and applicative
-- 2. being a functor means you can lift operation over the context
-- 3. being an applcative means:
--    1. You can shove any values into the context.
--    2. You can apply functions in a context / Function application with effects.
-- 4. being an monad means you can performance actions in sequence.
--    it's a consequence of nesting. When you have a structure as m (m a) and
--    reduce it to (m a), you need to remove the outer layer first.

-- normal bind:
(>>==) :: m a -> (a -> m b) -> m b
(>>==) = undefined

-- where join is:
join' :: Monad m => m (m a) -> m a
join' m = m >>= id


-- then:
join'' m = m >>=== id
  where
    -- imagine instantiate bind like this:
    -- m (m a) is just a (m b)
    (>>===) :: m (m a) -> (m a -> m a) -> m a
    (>>===) = undefined

-- monad law

-- 1. (return x) >>= f ≡ f x
--      shove a value into the context then bind to f is the same as apply f directly.
--      implication:
--        any value shoulbe be able to directly engage in a monadic operation by using pure.
leftId1 x f = do { x' <- return x;
                   f x'
                 }
leftId2 x f = do { f x }

-- 2. m >>= return ≡ m
--      shove m into return, which takes the value and put it back to a new monad.
--      implication:
--        bind a monad to return should have no effect.
rightId1 m = do { x <- m ;
                  return x
                }
rightId2 m = do { m }


-- 3. (m >>= f) >>= g ≡ m >>= (× -> f \x >>= g)
--      bind is associative.
--      implication:
--        the order to group mandic functions dosen't matter. You can: pick a part of the chain and make
--        it another function, then use the newly defined function in the same place, still works.
-- If a block has return value, then it of course all these three are equivalent:
assoc1 m f g = do { y <- do { x <- m;
                              f x
                            };
                    g y
                  }
assoc2 m f g = do { x <- m;
                    do { y <- f x ;
                         g y
                       }
                  }
assoc3 m f g = do { x <- m;
                    y <- f x;
                    g y
                  }

-- see how do block resembles a normal imperative code.
-- Monad law is important because the compiler inlines based on it. (at least compiler can rely on it
-- to perform code transformation).


{-@ Everything boilds down to functions:
      Programming is just programming, does matter how much fancy words you are using, everything goes down
      to some basic concepts. You perform instructions, you branch, you jump, you loop. No matter what you are using:
      a purely functional langauge or assembhly; these things are always around you.

      Though monad is a concept from cat theory, if it's widely applciable on programming, it must relates to
      those basic notaions some how. It's like, Arithmetics are also abstracted concepts. we talk about it because
      it models daily life situations nicely.

      Haskell has a fancy type system and a large set of syntax that makes it looks super complicated at the
      first glance. However, the fundation of all of these is just lambda calculus: It is still a lambda calculus based language.
      All abstractions you can play with essentially boil down to functions.

      Some exceptions like builtin types like integers and algebraic data types, which can also
      be encoded in lambda calculus. (chruch encoding, scott encoding etc.). At least, conceptually
      everything can be treated as functions, and thinking in this way makes a lot of thing much easier.
      (At least the language design is much more uniform, and the fundation is more thourough)

@-}

{-@
      A side note before goes into Monad: typeclass:
        what is typeclass?
          1. Typeclass consists two parts: type level constraint and term level function overloading.
          2. You can have typeclass with not term level definiton. Then it just says a type needs to be something.
          3. If a typeclass has term level funtion, you overload it. Overloading implies if you call the funcion
             on a value that is constrainted to be in typeclass, the right verision of the function will be invoked.
          4. typeclass help us to describe Monad eaiser, as the term level definition can to reflected to the type level.
          5. But really, it has nothing to do with monad itself. Nice thing to have but not necessasry.
          ok.

      So what's monad?

      when you see a type Monad m => m a, it means m is a monad,
        which means it supports at least >>= and return,
        which means it has a well defined way to remove a layer of structure and get the value out.

        NOTE: Given a monad you can join nested layers (join :: m (m a) -> m a),
          - the process of removing a layer of m implies you need to perform some computations.

        NOTE: we can define a helper function (>>= :: m a -> (a -> m b) -> m a) helps us to perform
              multiple computations on monads on after another. (chain them up!)

            we have m a and (a -> m b) as parameters. To run (a -> m b), we need to perform m a and get a.
            which means
              1. we need some how do (m a) -> a and get the a.
              2. we pass a as argument to (a -> m b)
              3. we get a new monad (m b) where >>= still applies.
            and
              step 1 perform the monad operation
              step 2 perform your arbitray operations based on the result of them monadic computation
@-}

-- put it here otherwise the comment is too long :)
-- Monad transfromer is still a monad.
instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing    -> return Nothing
      Just value -> runMaybeT $ f value

-- Monad requirements.
instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM


{-@
      what are implicit operations?
        Look at Maybe. you can see it as an side effect denotes success and failure. Or you can view merely
        by it's definiton: a container that can be any value or Nothing; when using it, it return some of the any value,
        or Nothing, base on some branching logics.
        The branching logic can be implied when chaining operations.

      what's the point of having implicit operations performed?

          - Get rid of boiler plates.
            if you write c, how do you handle errors?
            you write a fuction.
            you return a value, if it is not some magic value  (e.g #define ERR = -1), you take it as success. If it is,
            you handle it or propagate it.
              int foo(int a) {
                if (a < 0) {
                   return -1;
                }
                return sqrt(a);
              }

              int bar(int a) {
                if (a < 0 || isprime(a)) {
                   return -1;
                }
                return a;
              }

              int main() {
                int res = foo(-1);
                if (res) {
                  printf("error!");
                } else {
                  printf("meh, ok%d", res);
                }
                return 0;
              }

              The if else pattern for handling propagates everywhere, but it can be abstracted away.
              Inheritently, function foo and bar are functions that are possible to fail.
              The failure is singaled to it's called via return type.
              So what is a fuction that can fail?
                1. A function has an intended return type.
                2. The intended return type describe a set of intended return values.
                3. becuse the function maybe fail, it either return a intended value, or something
                  that can singal a failure. In another word, sucess or failure.
                4. As caller, if we receive the signal a failure signal, we know the computation failed,
                  so the whole computation needs to cease.
                  other wise, we can obtain a value of the intended type, the computation continue.
                  (this is the branching logic)
                5. This logic should be able to be chained up. Say I have functions f1, f2, f3 that all may
                  fail, I want to perform in order as f1 -> f2 -> f3. If any one of these functions signal a
                  failure, the computation stops. otherwise value propagates until we get the result.

              There are lots of choices to indicate a possible failure. The most used way is to use sum type.
              If something wrong, we indicate it as Nothing.
                (A thing to note: `Nothing` is the result of a computation that failed. it's a static representation of
                 failure. For the example above, bar fails as an consequence of the input being less than 0.
                 Implication:
                  1. caller can't know what's exactly wrong with the function,
                  2. Nothing itself is not a failure, it's a indicator of failure.)
              With the reprenstation, we can check if something goes wrong as a caller by check if the function return
              Just or Nothing:
                  run x =
                    case foo . bar $ x of
                      Just x -> putStrLn (show x)
                      Nothing -> putStrLn "peepeepoopoo"

              At this point it's just the same as C... Pattern matching looks fancy but it is still branching logic, you still
              need to do the checking by hand. What up?

              We hope we don't need to write the branching our self, instead just use the resulting value of a function that possible
              to fail. if anything goes wrong, evaluate the whole thing as Nothing, other wise proceed (point 4)
              In another word, we want the checking of Nothing to be implicit. Ah implicit! A monad!

              We just need to overload the bind (>>= :: m a -> (a -> m b) -> m b) for (Maybe a). In bind, when m a gives a, keep going,
              otherwise we return Nothing. This way, we only need to combine a (Maybe a)  with another function that returns a (Maybe b).
              In the function we can pretent (Maybe a) succeed. If it's failed, it's handled by (>>=).

              -- If we wrap the intended return value into another type that dedicated to handle errors, we can chain
              -- two may-failed operations with the same error handing logic.


          - Another view: chain Side effects:
              - what are side effects

                normally people give you examples like: mutation is side effect. performing io is side effect.
                but what's the difference between mutation and calcualte 1 + 2? what draws a line between side effect and
                non side effect?

                  We normally heard haskell functions are pure functions. Pure functions are referential transparent.

                  we know referential transparency: one input determines one output. f(1) = 1, f(1) = 2 is not a function.

                  but in c for example, lots of functions doesn't perform this way:
                    int foo(int x) {
                      int a;
                      scanf("%d\n", &a);
                      return a + x;
                    }
                  in this case, you don't know what gonna come with scanf. can you?
                  you can't! you can't predict the future, and there is no rule to govern how scanf work. From the view of out
                  program, the value of a comes from a different world!

              - how to represent side effects in a purely functional language?
                We don't need to explicit describe the sideffect in C, because purity is not part of it's design goal.
                so type of foo is just int -> int.

                However in haskell, all functions are pure. How do you make the return value of foo the same even the value Int
                change all the time?

                trick: We don't return Int anymore, we return a computation that yields Int.
                  thus we have foo :: Int -> IO Int

                This way, given any Int, we have the same value: A monad, a computation that gives a Int. the tricky part is in
                haskell we can't see the definition of IO, we only know it exists and it yields x.
                since the IO m is the same value, we keep the referential transparency. But what Int does IO Int give us is a
                different story.

              - So how is this related to monad?
                Given:
                  foo :: Int -> IO Int
                  bar :: Int -> Int -> IO Int
                  quux :: Int -> IO Int -> IO Int
                we know they are just "purified" functions that works on Int. In c they all have the same signature
               (quux might not, but doesn't matter now)

                But after we purified them, these function nolonger work with each other anymore. How to make it work again?
                we need to take the value in (IO Int) out (perform the IO action and get a Int), then use the Int as paramter
                for the next computation. Because we cannot directly control the process, this happens implicilty.
                What does it sounds like?

                if we make IO a monad, we can >>= it.
                  main :: IO Int
                  main = do
                    i <- foo 10
                    quux i (bar i 20)

                how does IO actually get performed? we don't care. at least in the language level it's not a concern.

            Can pure function has free variable?
              consider this case
                x = 10
                f y = x
              then f 1 = 10, f 2 = 10. is this a pure function?
              Is just a constant, the function application is redundent.
@-}

{-@
      What does it mean to be "in a monad m"
        Again let's use Maybe as example. With the notion of "possibly failed computation", we have
          1. A function that returns (Maybe a), the function can either succeed or fail. The return value record
             the senario of the execution process.
          2. A function that takes a (Maybe a), the parameter maybe valid or not valid.
          In


      Every monad has it's semantics.
        When talking about semantics you might think of the semantics of a programming langue in genral. But of course each
        component of a language has it's own semantics!

        Monad allows you to separate different semantics, and define them on their own.
          Maybe monad represents a computation that might fail, you can talk about monad now.
          Reader monad represents a computation that needs to use read in an environment, you can use it for passing implicit config.
          Pause Monad etc...

      What does it mean that monad allows you to describe sequencing?
        when you see a type (m (m a)), it can be thought as two layers of funcition stack on top of
        each other, each one perform some computations.

        then `join :: m (m a) -> m a` apparently needs to `call` the first layer to get rid of the first m!
        Essentially the only way to perform operations in sequence is by nesting.

@-}

-- some handy class
-- Alternative is like a monoid with different semantic.
instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing -> runMaybeT y
      Just _  -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

{-@ Monad is defined with join, but
    in haskell you get >>= instead.
    reason explained above.
@-}

-- kind signature helps you to tell the kindness of type variable
class (Applicative m) => Monadish (m :: * -> *) where
  merge :: m (m a) -> m a
  bind :: m a -> (a -> m b) -> m b
  bind m f = merge (fmap f m)
  {-# MINIMAL merge #-}

newtype Kleisli m a b = Kleisli (a -> m b)

kbind :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
kbind (Kleisli f) (Kleisli g) = Kleisli $ join . fmap g . f

-- bind' :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- bind' f g = join . fmap g . f

-- unsafePerformIO is a cokleisli arrow.
newtype Cokleisli w a b = Cokleisli (w a -> b)

class Functor w => Comonad w where
  (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
  extract :: w a -> a
