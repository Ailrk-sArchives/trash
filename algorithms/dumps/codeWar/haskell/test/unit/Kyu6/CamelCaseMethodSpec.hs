module Kyu6.CamelCaseMethodSpec where

import Kyu6.CamelCaseMethod
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf (printf)

spec :: Spec
spec = do
  it "test case" $
    camelCase "test case" `shouldBe` "TestCase"
  it "camel case method" $
    camelCase "camel case method" `shouldBe` "CamelCaseMethod"
  it "say hello" $
    camelCase "say hello " `shouldBe` "SayHello"
  it " camel case word" $
    camelCase " camel case word" `shouldBe` "CamelCaseWord"
