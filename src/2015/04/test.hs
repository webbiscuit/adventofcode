import Test.Tasty.Hspec
import Day04

main = hspec $ do
  describe "2015 Day 4" $ do

    context "Finds the first 5 zero'd bitcoin" $ do
      it "Passes the examples" $ do
        findHashThatMakesNZeroes 5 "abcdef" `shouldBe` "609043"
        findHashThatMakesNZeroes 5 "pqrstuv" `shouldBe` "1048970"
      