module Spec where


import UntypedArithmeticExpr
import Test.Hspec

main :: IO ()
main = do
  hspec $ describe "UntypedArithmeticExpr" untypedArithmeticExprSpec

untypedArithmeticExprSpec :: Spec
untypedArithmeticExprSpec = do
    it "eval> " $ do
      let expr1 = TmTrue
      let expr2 = TmIsZero TmZero
      let expr3 = TmIf TmFalse TmTrue TmFalse
      let expr4 = TmSucc $ TmSucc TmZero
      let expr5 = TmPred $ TmSucc TmZero
      eval expr1 `shouldBe` TmTrue
      eval expr2 `shouldBe` TmTrue
      eval expr3 `shouldBe` TmFalse
      eval expr4 `shouldBe` (TmSucc $ TmSucc TmZero)
      eval expr5 `shouldBe` TmZero

