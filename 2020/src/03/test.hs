import Test.Tasty.Hspec
import Day03

main = hspec $
  describe "2020 Day 3" $ do

    context "Can parse correctly" $
      it "Can parse simple maps" $
        parseMap "..##.......\n#...#...#.." `shouldBe` ["..##.......", "#...#...#.."]
      
    context "Can detect trees" $ do
      it "Can find a tree" $
        isTreeOnSpace "..##......." 2 `shouldBe` True

      it "Can find another tree" $
        isTreeOnSpace "..##......." 3 `shouldBe` True

      it "Can find a space" $
        isTreeOnSpace "..##......." 0 `shouldBe` False

      it "Can wrap around" $
        isTreeOnSpace "..##......." 13 `shouldBe` True

    context "Can count all trees on map" $ do
      it "Passes the Part1 example" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHit map `shouldBe` 7

      it "Passes the Part2 example" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHitWithAllStrategies map `shouldBe` 336

      it "Finds 2 trees with 1 1 strategy" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHitWithStrategy 1 1 map `shouldBe` 2

      it "Finds 7 trees with 3 1 strategy" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHitWithStrategy 3 1 map `shouldBe` 7

      it "Finds 5 trees with 1 1 strategy" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHitWithStrategy 5 1 map `shouldBe` 3

      it "Finds 4 trees with 7 1 strategy" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHitWithStrategy 7 1 map `shouldBe` 4

      it "Finds 2 trees with 1 2 strategy" $ do
        let map = [
                  "..##.......",
                  "#...#...#..",
                  ".#....#..#.",
                  "..#.#...#.#",
                  ".#...##..#.",
                  "..#.##.....",
                  ".#.#.#....#",
                  ".#........#",
                  "#.##...#...",
                  "#...##....#",
                  ".#..#...#.#"]
        countTreesHitWithStrategy 1 2 map `shouldBe` 2
