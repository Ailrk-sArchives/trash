{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module MiniProject.TicTacToe where
import           Control.Monad       (join, when)
import           Control.Monad.State
import           Data.Foldable       (fold)
import           Data.List           (subsequences, (\\))
import           Data.Maybe
import           Debug.Trace         (trace)


-- print and return
traceThis :: (Show a) => String ->  a -> a
traceThis msg x = trace (msg ++ " " ++ show x) x

{-@ Let's write a tic tac toe @-}
--  -OOO-
--  -XXX-


-- | Tictactoe game
-- v: phamtom type for solution
-- a: return type

newtype TicTacToe' v a = TicTacToe (Three -> Three -> a) deriving Functor
type TicTacToe v = TicTacToe' v (Maybe Bool)
type Board = [(Three, Three, Maybe Bool)]

-- (Z3, +)
data Three  = Zero | One | Two deriving (Eq, Show, Enum)


-- board
instance Semigroup (TicTacToe v) where
  TicTacToe a <> TicTacToe b = TicTacToe $ \x y ->
    case b x y of
      Nothing -> a x y
      Just v  -> Just v
instance Monoid (TicTacToe v) where mempty = TicTacToe . const . const $ Nothing


-- utils
showXO n | n = "X" | otherwise = "O"

instance Show (TicTacToe v) where
  show (TicTacToe b) = concatMap draw positions where
      positions = zip [1..9] [(n, m) | n <- enumFrom Zero,  m <- enumFrom Zero]
      draw (idx, (x, y)) = let marker =  maybe "_" showXO (b x y)
                            in if idx `mod` 3 == 0 then marker <> "\n" else marker

combinations :: Int -> [a] -> [[a]]
combinations = (. subsequences) . filter . (. length) . (==)

fullBoard :: TicTacToe v -> Board
fullBoard (TicTacToe b) = [(n, m, b n m) | n <- enumFrom Zero,  m <- enumFrom Zero]

maskBoard :: Bool -> Three -> Three -> TicTacToe v
maskBoard v x y = TicTacToe go
  where
    go i j | x==i && y==j = Just v
      | otherwise =  Nothing

printBoard :: TicTacToe v -> IO ()
printBoard = putStr . show

data KebabState = Kebab | NoKebab | NoMoreKebab  deriving (Show, Eq)

instance Semigroup Three where a <> b =  let [a', b'] = fromEnum <$> [a, b]
                                          in toEnum $ (a' + b') `mod` 3
instance Monoid Three where mempty = Zero

-- Three solutions to overload
data Symmetric
data LinAlg
data TreePrunning

class HasKebab a where
  type KebabStrategy a :: *
  hasKebab :: Bool -> a -> KebabState
  whatStrategy :: a -> String

-- | This implementation uses the fact that forall a, b \in  (Z3, +)
-- By rotating and fliping "shape" of the multiplication table, we can form D8. But because elements
-- in the table are symmetic along -- two diagnoses too, it turns out D and D' are the same as I and R180,
-- so we only have 6 unique elements in the group, call it G, G = D8 - {D, D'}.  for all x \in G,
-- sum of each straight column, row, or diagnose is always 0, we can use this property to test for solution
-- But there are some unwanted cases also sum up to zero. For example, say we have this table:
--     0 1 2, 1 2 0, 2 0 1
-- Case1: 0 1 _ In this case, 0 1 => 0 + 1 + 2 = 0 (mod 3). We can get rid of cases like this by also
--        _ 2 _ compute the sum of the same location in R90. If all pieces are in a row | column | diagnose,
--        _ _ _ the sum will be 0 in both tables.

rotateTicTacToe :: TicTacToe v -> TicTacToe v
rotateTicTacToe (TicTacToe d) = TicTacToe $ \i j ->
  case (i, j) of { (Zero, Zero) -> d Zero Two; (Zero, Two) -> d Two Two
                 ; (Two, Two) -> d Two Zero; (Two, Zero) -> d Zero Zero
                 ; (One, Two) -> d Two One; (Two, One) ->  d One Zero
                 ; (One, Zero) -> d Zero One; (Zero, One) -> d One Two
                 ; (a, b) -> d a b }
-- Case2: 0 _ _  This case is hard because it's symmetric along the diagnose. No matter how you rotate or flip
--        _ _ 0  the table the sum will always be 0. The best way I find so far is just manually exclude these
--        _ 0 _  cases. (There are four cases in total)

sameSet :: Eq a => [a] -> [a] -> Bool
sameSet x y = null (x \\ y) && null (y \\ x)

-- 00 12 21, 02 10 21, 20 01 12, 22 01 10
excludeDiagonalSymmetric :: [[(Three, Three)]]-> [[(Three, Three)]]
excludeDiagonalSymmetric = filter needToExclude
  where
    needToExclude xs | length xs /= 3 = False
      | and (fmap (sameSet xs) excludeList) = False
      | otherwise = True
    excludeList = [[(Zero, Zero), (One, Two), (Two, One)]
                  ,[(Zero, Two), (One, Zero), (Two, One)]
                  ,[(Two, Zero), (Zero, One), (One, Two)]
                  ,[(Two, Two), (Zero, One), (One, Zero)]]

instance HasKebab (TicTacToe Symmetric) where
  type KebabStrategy (TicTacToe Symmetric) = Symmetric
  hasKebab marker b | hasKebab' = Kebab
    | length [v | (_, _, v) <- fullBoard b, isNothing v] == 1 = NoMoreKebab
    | otherwise = NoKebab
    where
        roll f = join
               $ (fmap . fmap) (\(a, b) -> a <> b )
               $ excludeDiagonalSymmetric . combinations 3
               $ [ (x, y) | (x, y, v) <- fullBoard (f b), v == Just marker ]
        hasKebab' = elem (Zero, Zero) (zip (roll id) (roll rotateTicTacToe))
  whatStrategy _ = "Symmetic"

instance HasKebab (TicTacToe LinAlg) where
  type KebabStrategy (TicTacToe LinAlg) = LinAlg
  hasKebab = undefined
  whatStrategy _ = "LinAlg"

instance HasKebab (TicTacToe TreePrunning) where
  type KebabStrategy (TicTacToe TreePrunning) = TreePrunning
  hasKebab = undefined
  whatStrategy _ = "TreePrunning"


-- symmetric + exclude cases.
--
checkKebab :: Bool -> TicTacToe Symmetric -> KebabState
checkKebab marker b | hasKebab' = Kebab
  | length [v | (_, _, v) <- fullBoard b, isNothing v] == 1 = NoMoreKebab
  | otherwise = NoKebab
  where markers = [ x <> y | (x, y, v) <- fullBoard b, v == Just marker ]
        hasKebab' = Zero `elem` fold (combinations 3 markers)

-- linear algebra

-- tree and pruning

data GameState :: * -> * where
  GameState :: forall v. HasKebab (TicTacToe v) => { gameHistory :: [TicTacToe v]
                                                   , turn :: Bool
                                                   } -> GameState v

type GM v a = StateT (GameState v) IO a
newtype Game v a = Game { unGame :: GM v a }
  deriving newtype (Functor, Applicative, Monad, MonadState (GameState v), MonadIO, MonadFail)

tictactoeLoop :: HasKebab (TicTacToe v) => Game v ()
tictactoeLoop = do
      t <- gets turn
      liftIO . putStr $ showXO t <> "> "
      input <- liftIO readInput
      case input of
        [x, y] -> do
            games <- gets gameHistory
            let game = head games
                newgame = maskBoard t x y <> game
            liftIO . printBoard $ newgame
            put $ GameState (newgame : games) (not t)
            liftIO . print $ hasKebab t newgame
            case hasKebab t newgame of
              NoKebab     -> tictactoeLoop
              Kebab       -> liftIO . putStrLn $ (showXO t <> " Get kebab!!")
              NoMoreKebab -> liftIO . putStrLn $ "No more Kebab TAT"
  where
    readInput = fmap (toEnum . read) . words <$> getLine :: IO [Three]

runTicTacToe :: IO (GameState Symmetric)
runTicTacToe = execStateT (unGame tictactoeLoop) (GameState [mempty] False)
