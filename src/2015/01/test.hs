import Test.Tasty.Hspec
import Day01

main = hspec $ do
  describe "2015 Day 1" $ do

    context "Counting floors" $ do
      it "Passes the examples" $ do
        countFloors "(())" `shouldBe` 0
        countFloors "()()" `shouldBe` 0
        countFloors "(((" `shouldBe` 3
        countFloors "(()(()(" `shouldBe` 3
        countFloors "))(((((" `shouldBe` 3
        countFloors "())" `shouldBe` -1
        countFloors "))(" `shouldBe` -1
        countFloors ")))" `shouldBe` -3
        countFloors ")())())" `shouldBe` -3

    context "Entering basement" $ do
      it "Passes the examples" $ do
        firstBasementPosition ")" `shouldBe` Just 1
        firstBasementPosition "()())" `shouldBe` Just 5

      it "Should handle no basement" $ do
        firstBasementPosition "(" `shouldBe` Nothing