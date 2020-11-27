import Test.Tasty.Hspec
import Day13

main = hspec $
  describe "2019 Day 13" $
    context "Run part 1 tests" $ do
      it "Should count tiles" $ do
        let grid = createGrid [1,2,3,6,5,4]

        length grid `shouldBe` 2
        
      it "Should count have 1 ball" $ do
        let grid = createGrid [1,2,3,6,5,4]
        let ballCount = countTiles grid Ball

        ballCount `shouldBe` 1
