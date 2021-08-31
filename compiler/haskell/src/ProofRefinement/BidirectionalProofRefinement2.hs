module ProofRefinement.BidirectionalProofRefinement2 where

-- bidirectional refinement for type checking.
-- it's the same game but with a ast.

data Type = Nat | Prod Type Type | Arr Type Type deriving (Show, Eq)

data Expr = Var String
          | Ann Expr Type
          | Zero
          | Suc Expr
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          | Lam String Expr
          | App Expr Expr
          deriving Show

type Context = [(String, Type)]

data Judgement = Check Context Expr Type  -- need explicit type check
               | Synth Context Expr       -- simply synthesize
               deriving (Show)

-- rules needs to be type checked
decompoesCheck :: Context
               -> Expr
               -> Type
               -> Maybe ([Judgement], [Type] -> Maybe Type)
decompoesCheck g Zero Nat = Just ([], const $ Just undefined)
decompoesCheck g (Suc m) Nat = Just ([Check g m Nat], const $ Just undefined)
decompoesCheck g (Pair m n) (Prod a b) =
  Just ([Check g m a, Check g n b], const $ Just undefined)
decompoesCheck g (Lam x m) (Arr a b) =
  Just ([Check ((x, a) : g) m b], const $ Just undefined)
decompoesCheck g m a = Just ([Synth g m], \[a2] -> if a == a2 then Just undefined else Nothing)

-- rules that can be synthed directly
decomposeSynth :: Context
               -> Expr
               -> Maybe ([Judgement], [Type] -> Maybe Type)
decomposeSynth g (Var x) = lookup x g >>= \a -> return ([], const $ Just a)
-- type annotation. this force a check judegment.
decomposeSynth g (Ann m a) = return ([Check g m a], const $ Just a)
decomposeSynth g (Fst p) = return ([Synth g p], \[t] -> let (Prod a b) = t in Just a)
decomposeSynth g (Snd p) = return ([Synth g p], \[t] -> let (Prod a b) = t in Just b)
decomposeSynth g (App f x) =
  let cb = \[s, t] -> case s of
                        Arr a b | a == t -> Just b
                        _ -> Nothing
   in return ( [Synth g f, Synth g x] , cb)
decomposeSynth _ _ = Nothing

decompose :: Judgement -> Maybe ([Judgement], [Type] -> Maybe Type)
decompose (Check g m a) = decompoesCheck g m a
decompose (Synth g m) = decomposeSynth g m

data ProofTree = ProofTree Judgement [ProofTree] deriving (Show)

findProof :: Judgement -> Maybe (ProofTree, Type)
findProof j = do
  (js, f) <- decompose j
  tsas <- sequence (fmap findProof js)
  let (ts, as) = unzip tsas
  a <- f as
  return (ProofTree j ts, a)

-- >>> findProof (Synth [] (Ann (Suc (Suc (Zero))) (Prod Nat Nat)))
-- Nothing

-- >>> findProof (Synth [] (Ann (Suc (Suc (Zero))) Nat))
-- Just
-- (ProofTree (Synth [] (Ann (Suc (Suc Zero)) Nat))
--   [ProofTree (Check [] (Suc (Suc Zero)) Nat)
--     [ProofTree (Check [] (Suc Zero) Nat)
--        [ProofTree (Check [] Zero Nat) []]]],
--          Nat)

