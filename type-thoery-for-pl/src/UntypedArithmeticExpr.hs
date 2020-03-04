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

eval1 :: Term -> Term

eval1 term@(TmIf t1 t2 t3)
  | not $ isval t1 = TmIf (eval1 t1) t2 t3
  | otherwise =
      case term of
        TmIf TmTrue t2 t3  -> t2
        TmIf TmFalse t2 t3 -> t3

eval1 (TmSucc t1)
  | not $ isval t1 = TmSucc $ eval1 t1
  | otherwise = TmSucc t1

eval1 term@(TmPred t1)
  | not $ isval t1 = TmPred $ eval1 t1
  | otherwise =
      case term of
        TmPred TmZero     -> TmZero
        TmPred (TmSucc v) -> v

eval1 term@(TmIsZero t1)
  | not $ isval t1 = TmIsZero $ eval1 t1
  | otherwise =
      case term of
        TmIsZero TmZero     -> TmTrue
        TmIsZero (TmSucc _) -> TmFalse

eval1 n | isval n = n

eval1 _ = error "Unknown term"

eval :: Term -> Term
eval t = let t' = eval1 t
          in eval t'
