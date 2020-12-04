import Test.Tasty.Hspec
import Day04

main = hspec $
  describe "2020 Day 4" $ do

    context "Can parse correctly" $
      it "Can parse simple maps" $
        parseMap "..##.......\n#...#...#.." `shouldBe` ["..##.......", "#...#...#.."]
 