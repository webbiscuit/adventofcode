import Test.Tasty.Hspec
import Day01

main = hspec $
  describe "2020 Day 1" $ do

    context "Finding 2020 expense (doubles)" $ do
      it "Example works" $
        find2020Expense [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 514579

      it "Single item lists return Nothing" $
        find2020Expense [1721] `shouldBe` Nothing

      it "Empty lists return Nothing" $
        find2020Expense [] `shouldBe` Nothing

      it "Unmatched lists return Nothing" $
        find2020Expense [1234, 456] `shouldBe` Nothing

    context "Finding 2020 expense (triples)" $ do
      it "Example works" $
        find2020TriplesExpense [1721, 979, 366, 299, 675, 1456] `shouldBe` Just 241861950

      it "Single item lists return Nothing" $
        find2020TriplesExpense [1721] `shouldBe` Nothing

      it "Empty lists return Nothing" $
        find2020TriplesExpense [] `shouldBe` Nothing

      it "Unmatched lists return Nothing" $
        find2020TriplesExpense [1234, 456] `shouldBe` Nothing
      
