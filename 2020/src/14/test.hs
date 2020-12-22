import Test.Tasty.Hspec
import Day14

main = hspec $
  describe "2020 Day 14" $

    context "Can pass the example" $
      it "Part 1 simple example" $ do
        input <- readFile "example.txt"
        let instructions = parse input
        let memory = initialiseMemory instructions

        sumMemoryValues memory `shouldBe` 165

