import Test.Tasty.Hspec
import Day07
import Data.Word

main = hspec $
  describe "2015 Day 7" $

    context "Doing Logic" $ do
      it "Assigns with 123 -> x" $
        readVariable "x" (parseStatements ["123 -> x"]) `shouldBe` Just 123

      it "ANDs x AND y -> d" $
        readVariable "d" (parseStatements ["123 -> x","456 -> y","x AND y -> d"]) `shouldBe` Just 72

      it "ORs x OR y -> d" $
        readVariable "d" (parseStatements ["123 -> x","456 -> y","x OR y -> d"]) `shouldBe` Just 507

      it "x LSHIFT 2 -> d" $
        readVariable "d" (parseStatements ["123 -> x","x LSHIFT 2 -> d"]) `shouldBe` Just 492

      it "x RSHIFT 2 -> d" $
        readVariable "d" (parseStatements ["123 -> x","x RSHIFT 2 -> d"]) `shouldBe` Just 30

      -- it "NOT x -> d" $
      --   readVariable "d" (parseStatements ["123 -> x","NOT x -> d"]) `shouldBe` Just -124

      -- it "1 AND x -> d" $
      --   readVariable "d" (parseStatements ["123 -> x","1 AND x -> d"]) `shouldBe` Just 1
