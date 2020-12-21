import Test.Tasty.Hspec
import Day11

main = hspec $
  describe "2020 Day 11" $

    context "Can pass the examples" $
      it "Part 1 simple example" $ do
        input <- readFile "example.txt"
        let seatLayout = moveUntilStable $ parse input
        countOccupiedSeats seatLayout `shouldBe` 37




