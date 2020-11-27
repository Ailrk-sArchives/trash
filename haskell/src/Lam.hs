module Lam where

-- Some basic lambda calculus.


{-@ Lambda calculus (λx.x)(λy.y)

    definition of lambda expressions
      e := x      (Var) variable
           λx.e   (Lam) abstraction
           e e    (App) application

    convention for multiple parameters
      λxy.z = λx.λy.z

    A variable is bound if it is contained within the parameter of abstraction
    A variable is free if it is not bound.
    An abstraction with only bound variable is called a combinator.
      e₀ = λx.x           -- this is a combinator
      e₁ = λx.x(λy.ay)y   -- this is not a combinator

    Variables that appears multiple times are ound by the inner most binder.
      λxy.(λxz.x+y)   -- inner x has nothing to do with outer x.
@-}

type Name = String

-- lambda calculus is really simple...
data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving (Eq, Show)

-- this is not part of lambda calculus, but it's convenient shorthand.
data Lit = LInt Int
         | LBool Bool
         deriving (Eq, Show)

{-@ SKI combinator

    S = λf.λg.λx.fx(gx)
    K = λx.λy.x
    I = λx.x

    I is actually not necessary because
      SKK = (λf.λg.λx.fx(gx))(λx.λy.x)(λx.λy.x)
          = (λg.λx.((λa.λb.a))x(gx))(λx.λy.x)
          = λx.(λa.λb.a)x((λa.λb.a)x))
          = λx.(λa.λb.a)x((λa.λb.a)x))
          = λx.(λb.x)(λb.x)
          = λx.(λb.x)(x)
          = λx.x
          = I
          ∎

    ω combinator ω = λx.xx
      let Ω = ωω, we find the result is a infinite chain of omega.
      many statically typed typesystem will reject this term fro being well formed.
      It's useful for testing the validity of the typesystem.
@-}

-- define some ski combinator
s f g x = f x (g x)
k x _ = x
i = s k k

-- ω is rejected by the type system.

{-@ Rules of lambda calculus

    * Substitution

      (λx.e)a → [x/a]e

      The notation means application is simply replace
      all occurences of x in expression e with a.

    * Name capturing.
      One of the biggest bummber of lambda calculus is the
    for example:

      [y/x](λy.yx) → λx.xx

      this is not correct. Because you cannot just replace y to x since
      x alreay means a free variable.

    * Solution of name capturing
      use capture-avoiding substitution.
      Ban substitute to the same name as free variable

        (λx.e)a → [x/a]e | x ∉ fv(e)

    * Conversion and Equivalences
        * Alpha equivelance.
                 α
          (λx.e) = (λy.[x/y]e)

          Simply replace variable on the binder and the body of expression
          will not change the meaning ofthe expression.

        * Beta reduction
                  β
          (λx.a)y → [x/y]a
          Perform substitution once.

        * Eta reduction
                  η
          (λx.e)x → e if x ∉ fv(e)
          Get rid of the useless parameter.
@-}


{-@

@-}
