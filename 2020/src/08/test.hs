import Test.Tasty.Hspec
import Day08

main = hspec $
  describe "2020 Day 8" $

    context "Can pass the examples" $
      it "Part 1 example terminates at first loop with 5" $ do
        program <- readFile "example.txt"
        readAccumulator (runProgram program) `shouldBe` 5



