module demo.demo1 where

data Bool : Set where
  true : Bool
  false : Bool

-- \l reload.
-- \c split into cases
-- \g put an expression in the hole {! !} ahead of time
--    and it will automatically fill the expression if it
--    type checks.
if_then_else_ : {a : Set} → Bool → a → a → a
if true then t else f = t
if false then t else f = f

-- \a search for an answer
not : Bool → Bool
not true = false
not false = true

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- \r refine. make sure expression typechecks
--    if not add some extra holes to make it typecheck
--    it's a more powerful give
_+_ : ℕ → ℕ → ℕ
zero + y = y
suc x + y = suc (x + y)


data Vec (a : Set) : ℕ → Set where
  nil : Vec a zero
  cons : {n : ℕ} → a → Vec a n → Vec a (suc n)

-- {! -c !} let the auto command also do case analysis
-- in this case it solve the problem completely.
vconcat : {a : Set} {m n : ℕ} → Vec a m → Vec a n → Vec a (m + n)
vconcat {_} {zero} v1 v2 = v2
vconcat {_} {suc x} (cons x₁ x₂) v2 = cons x₁ (vconcat x₂ v2)

-- create a vector
one = suc zero
two = one + one
three = one + two
four = one + three
five = one + four
six = one + five
seven = one + six
eight = one + seven
nine = one + eight
ten = one + nine

-- normalize to long long expression
vec1 = cons zero (cons (ten + ten) (cons six (cons five nil)))

-- normalize with \n
-- give you the normalized form of the expression
-- \n high will normalize high to (suc (suc zero))
--
-- \e gives the types in the context.
low = suc zero
high = low + low
