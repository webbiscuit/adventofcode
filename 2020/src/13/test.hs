import Test.Tasty.Hspec
import Day13

main = hspec $
  describe "2020 Day 13" $

    context "Can pass the example" $
      it "Part 1 simple example" $ do
        input <- readFile "example.txt"
        let (time, buses) = parse input
        let bus = findEarliestBus time buses
        bus `shouldBe` (59, 944)

        calcWaitForBus time bus `shouldBe` 295

