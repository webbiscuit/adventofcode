import Test.Tasty.Hspec
import Day10

main = hspec $
  describe "2020 Day 10" $

    context "Can pass the examples" $ do
      it "Part 1 simple example" $ do
        input <- readFile "example.txt"
        let joltages = calculateJoltageDifferences $ prepare $ parse input
        countJoltageDifference joltages 1 `shouldBe` 7
        countJoltageDifference joltages 3 `shouldBe` 5

      it "Part 1 bigger example" $ do
        input <- readFile "example2.txt"
        let joltages = calculateJoltageDifferences $ prepare $ parse input
        countJoltageDifference joltages 1 `shouldBe` 22
        countJoltageDifference joltages 3 `shouldBe` 10



