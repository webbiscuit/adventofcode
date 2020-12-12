import Test.Tasty.Hspec
import Day12

main = hspec $
  describe "2020 Day 12" $

    context "Can pass the example" $ do
      it "Part 1 simple example" $ do
        input <- readFile "example.txt"
        let instructions = parse input
        let position = findPosition instructions
        let distance = calculateManhattanDistance (0,0) position
        position `shouldBe` (17, -8)
        distance `shouldBe` 25

      it "Part 2 simple example" $ do
        input <- readFile "example.txt"
        let instructions = parse input
        let position = findPositionUsingWaypoint (0,0) (10, 1) instructions
        let distance = calculateManhattanDistance (0,0) position
        position `shouldBe` (214, -72)
        distance `shouldBe` 286

