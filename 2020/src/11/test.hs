import Test.Tasty.Hspec
import Day11

main = hspec $
  describe "2020 Day 11" $

    context "Can pass the examples" $ do
      it "Part 1 example" $ do
        input <- readFile "example.txt"
        let seatLayout = moveUntilStable $ parse input
        countOccupiedSeats seatLayout `shouldBe` 37

      it "Part 2 straight example" $ do
        input <- readFile "simpleStraight.txt"
        let seatLayout = moveUntilStable $ parse input
        getViewableNeighbours seatLayout (1, 1) `shouldBe` [OccupiedSeat]

      it "Part 2 blindspot example" $ do
        input <- readFile "simpleBlindspot.txt"
        let seatLayout = moveUntilStable $ parse input
        getViewableNeighbours seatLayout (3, 3) `shouldBe` []

      it "Part 2 example" $ do
        input <- readFile "example.txt"
        let seatLayout = moveUntilStableUsingLooking $ parse input
        countOccupiedSeats seatLayout `shouldBe` 26




