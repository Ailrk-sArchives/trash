{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module ClosureConversion () where

import           Control.Applicative          (liftA2)
import           Control.Monad.Trans.State    as State
import           Data.Char                    (isDigit, isLetter, isSpace)
import qualified Data.HashMap.Lazy            as HM
import qualified Data.HashSet                 as HS
import           Data.Maybe                   (fromJust)
import           Data.Typeable
import           Text.ParserCombinators.ReadP as P

type Var = String

data Expr r where
  Var :: Var -> Expr k
  Apply :: Expr expr0 -> [Expr expr1] -> Expr k
  Lambda :: [Expr Var] -> Expr body -> Expr k

-- closure conversion only
  LambdaConverted :: [Expr Var] -> Expr body -> Expr k
  MkClosure :: Expr lam -> Expr env -> Expr k
  MkEnv :: [(Expr var, Expr expr)] -> Expr k
  EnvRef :: Expr env -> Expr ref -> Expr k
  ApplyClosure :: Expr expr0 -> [Expr expr1] -> Expr k

data SomeExpr where
  SomeExpr :: forall r. (Expr r) -> SomeExpr

unwrapSomeExpr :: SomeExpr -> forall r. (forall a. Expr a -> r) -> r
unwrapSomeExpr (SomeExpr expr) k = k expr

deriving instance Show (Expr r)
deriving instance Typeable (Expr r)

fromVar :: Expr Var -> Var
fromVar (Var n) = n
fromVar _       = error "no way"

-------------------------------------------------------------------------------
-- parser

identifierp = idp <* skipSpaces
  where
    idp = do
      first <- satisfy isLetter
      rest <- munch (\c -> isDigit c || isLetter c)
      return $ first : rest

-- >>> P.readP_to_S reservedp "lambda"

tokenp :: Char -> ReadP Char
tokenp c = char c <* skipSpaces

reserved :: String -> ReadP String
reserved s = string s <* skipSpaces

varp :: P.ReadP (Expr r)
varp = Var <$> identifierp

-- (f as)
applyp :: P.ReadP (Expr r)
applyp = do
  tokenp ('(')
  Apply <$> parse <*> manyTill parse (tokenp ')')

lambbdaBodyp :: ([Expr Var] -> Expr a -> Expr k) -> ReadP b -> ReadP (Expr k)
lambbdaBodyp con prefix = prefix *> (tokenp '(') *> do
  ts <- manyTill identifierp (tokenp ')')
  expr <- parse
  return $ con (fmap Var ts) expr

lambdap :: P.ReadP (Expr r)
lambdap = lambbdaBodyp Lambda (reserved "lambda")

lambdaConvertedp :: P.ReadP (Expr r)
lambdaConvertedp = lambbdaBodyp LambdaConverted (reserved "lambda*")

mkClosurep :: P.ReadP (Expr r)
mkClosurep = MkClosure <$> (reserved "mkclosure" *> parse) <*> parse

mkEnvp :: P.ReadP (Expr r)
mkEnvp = MkEnv <$> (reserved "mkenv" *> pairsp)
  where
    pairp = (,) <$> (tokenp '(' *> varp) <*> (parse <* tokenp ')')
    pairsp = tokenp '(' *> (manyTill pairp (tokenp ')'))

envRefp :: P.ReadP (Expr r)
envRefp = EnvRef <$> (reserved "envref" *> varp) <*> varp

applyClosurep :: P.ReadP (Expr r)
applyClosurep = ApplyClosure
            <$> (reserved "apply-closure" *> parse)
            <*> (manyTill parse (tokenp ')'))

parse :: P.ReadP (Expr r)
parse = parse1 <++ applyp
  where
    parse1 = lambdaConvertedp
         <++ lambdap
         <++ mkClosurep
         <++ mkEnvp
         <++ envRefp
         <++ varp
         <++ applyClosurep
         <++ between (tokenp '(') (tokenp ')') parse

-- >>> let c1 = "(mkclosure (lambda (env x) (envref env a)) (mkenv ((f f))))"
-- >>> readP_to_S parse c1
-- >>> readP_to_S parse "(lambda (x) (lambda (y) (x y)))"

type UniqueId = Int
type LC a = State UniqueId a

getUniqueId :: LC Int
getUniqueId = do
  i <- State.get
  modify $ \i -> i + 1
  return i

free :: Expr r -> HS.HashSet Var
free (Lambda params body) =
  HS.difference (free body) (HS.fromList (fmap fromVar params))
free (LambdaConverted params body) =
  HS.difference (free body) (HS.fromList (fmap fromVar params))
free (Var v)             = HS.singleton v
free (MkClosure lam env) =  HS.union (free lam) (free env)
free (MkEnv env@(((v, e):_))) = HS.unions (fmap free (fmap snd env))
free (ApplyClosure f vs) = HS.unions (free f : fmap free vs)
free (Apply f vs) = HS.unions (free f : fmap free vs)

-- | substitute free variables with dictionary
substitue :: HM.HashMap Var (Expr r) -> Expr c -> Expr r
substitue dict (Lambda params body) = undefined
substitue dict (LambdaConverted params body) = undefined
substitue dict expr@(Var v)
  | v `HM.member` dict = dict HM.! v
  | otherwise = undefined

-- undefined -- TOOD @ two branches return different types.
substitue dict (Apply lam params) =
  Apply (substitue dict lam) (fmap (substitue dict) params)
substitue dict (MkClosure lam env) =
  MkClosure (substitue dict lam) (substitue dict env)
substitue dict (MkEnv env) = MkEnv (fmap (\(v,e) -> (v, substitue dict e)) env)
substitue dict (EnvRef env ref) = EnvRef (substitue dict env) ref
substitue dict (ApplyClosure lam params) =
  ApplyClosure (substitue dict lam) (fmap (substitue dict) params)

closureConvert :: Expr r -> Expr r
closureConvert expr = evalState (convert expr) 0
  where
    convert :: Expr r -> LC (Expr r)
    convert expr@(Lambda params body) = do
      let fv = free expr
          env = fmap (\v -> (Var v, Var v)) (HS.toList fv)
      envSym <- genEnvSym
      sub <- do
          envSym <- genEnvSym
          let mkref k = EnvRef (Var envSym) (Var k)
          return $ HM.mapWithKey (\k _  -> mkref k) (HS.toMap fv)
      let body' = substitue sub body
      params' <- return ((Var envSym) : params)
      return $ MkClosure (LambdaConverted params body') (MkEnv env)
      where
        genEnvSym = (\i -> "env" ++ show i) <$> getUniqueId

    convert (Apply f args) = return $ ApplyClosure f args
    convert expr = return expr

-- transformBottomUp :: (Expr r -> Expr r) -> Expr r -> Expr r
-- transformBottomUp f expr = f (transform expr)
--   where
--     t e = transformBottomUp f e
--     transform :: Expr r -> Expr k
--     transform (Lambda params body) = Lambda params (t body)
--     transform expr@(Var _) = expr
--     -- transform (Apply eexpr0 l_eexpr1) = _
--     -- transform (LambdaConverted l_el_c ebody) = _
--     -- transform (MkClosure elam eenv) = _
--     -- transform (MkEnv l_p_evareexpr) = _
--     -- transform (EnvRef eenv eref) = _
--     -- transform (ApplyClosure eexpr0 l_eexpr1) = _


transformTopdown :: (Expr r -> Expr r) -> Expr r -> Expr r
transformTopdown = undefined

flatClosureConvert :: Expr r -> Expr r
flatClosureConvert = transformBottomUp closureConvert

sharedClosureConvert :: Expr r -> Expr r
sharedClosureConvert = transformTopdown closureConvert
