import Test.Tasty.Hspec
import Day08

main = hspec $
  describe "2020 Day 8" $

    context "Can pass the examples" $ do
      it "Part 1 example terminates at first loop with 5" $ do
        code <- readFile "example.txt"
        runProgram (parseToProgram code) `shouldBe` (Infinity, 5)

      it "Part 2 example terminates with no loops with 8" $ do
        code <- readFile "example-ends.txt"
        runProgram (parseToProgram code) `shouldBe` (Eof, 8)

      it "Part 2 example can heal itself 8" $ do
        code <- readFile "example.txt"
        healProgram (parseToProgram code) `shouldBe` (Eof, 8)



