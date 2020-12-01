import Test.Tasty.Hspec
import Day01

main = hspec $
  describe "2020 Day 1" $ do

    context "Finding 2020 expense (doubles)" $ do
      it "Example works" $
        find2020Expense [1721, 979, 366, 299, 675, 1456] `shouldBe` 514579

      it "Single item lists return 0" $
        find2020Expense [1721] `shouldBe` 0

      it "Empty lists return 0" $
        find2020Expense [] `shouldBe` 0

      it "Unmatched lists return 0" $
        find2020Expense [1234, 456] `shouldBe` 0

    context "Finding 2020 expense (triples)" $ do
      it "Example works" $
        find2020TriplesExpense [1721, 979, 366, 299, 675, 1456] `shouldBe` 241861950

      it "Single item lists return 0" $
        find2020TriplesExpense [1721] `shouldBe` 0

      it "Empty lists return 0" $
        find2020TriplesExpense [] `shouldBe` 0

      it "Unmatched lists return 0" $
        find2020TriplesExpense [1234, 456] `shouldBe` 0
      
