import Test.Tasty.Hspec
import Day09

main = hspec $
  describe "2020 Day 9" $

    context "Can pass the examples" $
      it "Part 1 example terminates at first loop with 5" $ do
        code <- readFile "example.txt"
        let parsed = parse code
        findXmasWeakness parsed 5 `shouldBe` 127



