import Test.Tasty.Hspec
import Test.Hspec.Tables
import Day05

main = hspec $
  describe "2020 Day 5" $ do

    context "Can calculate ticket ids" $
      byExample
        ("ticket", "result")
        [ 
          ("FBFBBFFRLR", 357),
          ("BFFFBBFRRR", 567),
          ("FFFBBBFRRR", 119),
          ("BBFFBBFRLL", 820)
        ]
        (\a expected -> calculateSeatId a `shouldBe` expected)

    context "Can find missing seat id" $
      byExample
        ("seat ids", "result")
        [ 
          ([1,2,4,5], Just 3),
          ([99,100,102], Just 101),
          ([99,100,101], Nothing)
        ]
        (\a expected -> findMissingSeatId a `shouldBe` expected)