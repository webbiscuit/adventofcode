import Test.Tasty.Hspec
import Day02

main = hspec $ do
  describe "2015 Day 2" $ do

    context "Counting paper" $ do
      it "Passes the examples" $ do
        calculateTotalWrappingPaperNeeded "2x3x4" `shouldBe` 58
        calculateTotalWrappingPaperNeeded "1x1x10" `shouldBe` 43

    context "Counting ribbon" $ do
      it "Passes the examples" $ do
        calculateTotalRibbonNeeded "2x3x4" `shouldBe` 34
        calculateTotalRibbonNeeded "1x1x10" `shouldBe` 14