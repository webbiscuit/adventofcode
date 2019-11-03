import Test.Tasty.Hspec
import Day07
import Data.Word

main = hspec $
  describe "2015 Day 7" $

    context "Doing Logic" $
      it "Assigns with 123 -> x" $
        readVariable "x" (parseStatements ["123 -> x"]) `shouldBe` Just 123