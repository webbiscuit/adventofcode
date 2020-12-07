import Test.Tasty.Hspec
import Day07

main = hspec $
  describe "2020 Day 7" $ do

    context "Can count bags" $
      it "Finds 4 bags in example" $ do
        bagRules <- readFile "example.txt"
        length (bagsContainingBag (lines bagRules) "shiny gold") `shouldBe` 4

    context "Can Parse correctly" $ do

      it "Can parse a line with one bag type" $
        parseBagRule "faded tan bags contain 4 dim brown bags."  
          `shouldBe` 
          ("faded tan", [("dim brown", 4)])

      it "Can parse a line with many bags" $
        parseBagRule "striped orange bags contain 1 vibrant green bag, 5 plaid yellow bags, 1 drab magenta bag"  
          `shouldBe` 
          ("striped orange", [("vibrant green", 1), ("plaid yellow", 5), ("drab magenta", 1)])

      it "Can parse a line with no containing bags" $
        parseBagRule "dotted black bags contain no other bags."  
          `shouldBe` 
          ("dotted black", [])

