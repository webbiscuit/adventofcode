import Test.Tasty.Hspec
import Day08

main = hspec $
  describe "2019 Day 8" $ do

    context "Merge layers" $
      it "0222112222120000 merges to 0110" $
        mergeLayers (RawImage 2 2 (parseInput "0222112222120000")) `shouldBe` parseInput "0101"

    context "Counts layers" $
      it "123456789012 has 2 layers" $
        length (calculateLayers $ RawImage 3 2 (parseInput "123456789012")) `shouldBe` 2
