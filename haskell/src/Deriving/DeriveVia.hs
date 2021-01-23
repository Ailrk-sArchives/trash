{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Deriving.DeriveVia where



import           Data.Coerce

{-@ Equality constraint and Coercible constraint
    To understand derive via, first you need to now how
    coerce works. (coercible constraint)
    But to understand coercible constraint works, you need to know
    how equality constraint works...

    First, type context is the current environment, all our types are
    in the environment. You can think it as a set S
    let's say forall t1, t2 \in S,  t1 ~ t2
    This means t1, t2 need to be the same
@-}


-- use functional dependencies to add unique dependency constraint.
class C' a b | a -> b

-- use equality constraint to say a map of a with F should be the same
-- as b, which means a functional dependency.
class (F a ~ b) => C a b where
  type F a




{-@ Coerce
    The coerce function in Data.Coerce has nothing to do with
    C++ style coercion, it only does safe coerce.
    Namely coerce :: Coerciable a b => a -> b.
    Thus to be able to coerce a to b, there must exist a instance
    Coercible instance between a and b.

 @-}
