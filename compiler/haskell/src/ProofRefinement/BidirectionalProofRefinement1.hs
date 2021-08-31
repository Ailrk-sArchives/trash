module ProofRefinement.BidirectionalProofRefinement1 where

-- Problem:
-- Sometimes a jugement only have partial information. We want to refine
-- in those cases.

data Nat = Zero | Suc Nat deriving (Show, Eq)

-- Idea:
-- instead of having e.g Plus a b c as a three argument judgement, with
-- a b be arguments c be the result, and just map from jugement to results,
-- we think different paratemers with different modes. e.g a b being input mode
-- and c being output mode.
--
-- This way we don't need all info presented in the judgement. We can carry
-- on the refinement with partial information, until it gets some subresults.
-- Because we don't have all result, we need to use these subresults to get
-- full result. To do that we pass a continuation to consume subresults.
--
-- Thus bidirecional.


data Judgement = Plus12 Nat Nat  -- + a b _
               | Plus13 Nat Nat  -- + a _ c
               deriving Show

-- + a b _
-- this one is deterministic. You can always get another Nat by adding two
-- nats.
decompose12 :: Nat -> Nat -> Maybe ([Judgement], [Nat] -> Nat)
decompose12 Zero y    = Just ([], \_ -> y)
-- remove a suc on x add it to z
decompose12 (Suc x) y = Just ([Plus12 x y], \[z] -> Suc z)

-- + a _ c
-- this one is partial, e.g plus 3 _ 2 is not defined for Nat.
decompose13 :: Nat -> Nat -> Maybe ([Judgement], [Nat] -> Nat)
decompose13 Zero z          = Just ([], \_ -> z)
decompose13 (Suc x) (Suc z) = Just ([Plus12 x z], \[x] -> Suc x)
decompose13 _ _             = Nothing

decompose (Plus12 n m) = decompose12 n m
decompose (Plus13 n m) = decompose13 n m

data ProofTree = ProofTree Judgement [ProofTree] deriving Show

findProof :: Judgement -> Maybe (ProofTree, Nat)
findProof j = do
  (js, f) <- decompose j
  tns <- sequence (fmap findProof js)
  let (ts, ns) = unzip tns
  return (ProofTree j ts, f ns)

-- >>> findProof (Plus12 Zero (Suc (Suc Zero)))
-- Just (ProofTree (Plus12 Zero (Suc (Suc Zero))) [], Suc (Suc Zero))

-- >>> findProof (Plus12 (Suc Zero) (Suc (Suc Zero)))
-- Just (ProofTree (Plus12 (Suc Zero) (Suc (Suc Zero)))
--        [ProofTree (Plus12 Zero (Suc (Suc Zero))) []],
--          Suc (Suc (Suc Zero)))

--- >>> findProof (Plus13 (Suc Zero) (Suc (Suc Zero)))
-- Just (ProofTree (Plus13 (Suc Zero) (Suc (Suc Zero)))
--        [ProofTree (Plus12 Zero (Suc Zero)) []],
--          Suc (Suc Zero))
