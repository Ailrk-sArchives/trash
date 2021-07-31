module Other.RecursionScheme1 where

import           Control.Arrow

-- Fix Point Of Functor


-- first, a syntax tree
data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

-- parameterize the occurrence of Expr itself.
-- replace Expr with a.
data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq)


apply :: (a -> b) -> Expr a -> Expr b
apply f (Index e1 e2)   = Index (f e1) (f e2)
apply f (Call x xs)     = Call (f x) (fmap f xs)
apply f (Unary op x)    = Unary op (f x)
apply f (Binary x op y) = Binary (f x) op (f y)
apply f (Paren x)       = Paren (f x)
apply f (Literal l)     = Literal l


-- our syntax tree now actually is a functor.
instance Functor Expr where
  fmap = apply



{-@ Get the fix point of Expr a with Term.
    Why you want this?
    We can isolate the definition of each expression and nested
    expression.
@-}

data Term f = In (f (Term f))

-- out is a helper function to evaluate the Term f
out :: Term f -> f (Term f)
out (In t) = t
-- see in action how our new recursive type accomonate all nested combinations of
-- Expr:

n1 = let term = undefined :: Term Expr
      in out term

n2 = let term = In (Literal (IntLit 2))
      in out term

n3 = let term = In (Paren (In (Literal (IntLit 2))))
      in out term

-- ok this also works...
n4 = let term = In (Index (In (Literal (IntLit 3)))
                          (In (Paren (In (Literal (StrLit "str"))))))
      in out term

-- wow this stil works (of course!)
n5 = let term = In (Binary (In (Paren (In (Literal (IntLit 3)))))
                           "+"
                           (In (Binary (In (Binary (In (Literal (IntLit 3)))
                                           "*"
                                           (In (Literal (IntLit 10)))))
                                        "-"
                                        (In (Literal (IntLit 10))))))
      in out term

{-@ Note for Term f, f is a functor

    So Term is a fix point of the functor f.

    To generialize the traversal of the recursive data type.
    To traverse, we can roughly do these things:
    1. unpack Term to access all it's children (with the out function).
    2  recursively apply f to all children
    3. repack Term
    4. apply f to term.
@-}

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f

flattern :: Term Expr -> Term Expr
flattern (In (Paren e)) = e
flattern other          = other

n6 = let term = In (Paren (In (Binary (In (Paren (In (Literal (IntLit 3)))))
                               "+"
                               (In (Binary (In (Binary (In (Literal (IntLit 3)))
                                               "*"
                                               (In (Literal (IntLit 10)))))
                                            "-"
                                          (In (Literal (IntLit 10))))))))
      in bottomUp flattern term

-- what about traverse the type top down?

{-@ To traverse it topdown.
    1. apply f to Term
    2. unpack Term to get children
    3. recursively apply f to it's children
    4. pack Term back
@-}

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f = In <<< fmap (topDown f) <<< out <<< f
