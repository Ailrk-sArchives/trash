module UntypedArithmeticExpr where

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Show, Eq)

isval :: Term -> Bool
isval term =
  case term of
    TmTrue   -> True
    TmFalse  -> True
    TmZero   -> True
    TmSucc _ -> True
    _        -> False

-- single evaluation
eval1 :: Term -> Term

-- if true then t2 else t3 -> t2
-- if false then t2 else t3 -> t3
--                t1 -> t1'
-- -------------------------------------------------
--  if t1 then t2 else t3 -> if t1' then t2 else t3
eval1 (TmIf t1 t2 t3) | not $ isval t1 = TmIf (eval1 t1) t2 t3
eval1 (TmIf TmTrue t2 t3) = t2
eval1 (TmIf TmFalse t2 t3) = t3

--       t1 -> t1'
-- ---------------------
--   succ t1 -> succ t1'
eval1 (TmSucc t1)
  | not $ isval t1 = TmSucc $ eval1 t1
  | otherwise = TmSucc t1

-- pred 0 -> 0
-- pred (succ v) -> v
--       t1 -> t1'
-- ---------------------
--   pred t1 -> pred t1'
eval1 (TmPred t1) | not $ isval t1 = TmPred $ eval1 t1
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc v)) = v

-- iszero 0 -> true
-- iszero (succ v) -> false
--       t1 -> t1'
-- ---------------------
--   iszero t1 -> iszero t1'
eval1 (TmIsZero t1) | not $ isval t1 = TmIsZero $ eval1 t1
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc _)) = TmTrue

-- const(t)
eval1 n | isval n = n

eval1 _ = error "Unknown term"

-- eval motor
eval :: Term -> Term
eval t = let t' = eval1 t in if isval t' then t' else eval t'
