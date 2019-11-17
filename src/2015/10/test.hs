import Test.Tasty.Hspec
import Day10

main = hspec $
  describe "2015 Day 10" $

    context "Calculating look-and-say" $ do
      it "1 becomes 11" $
        lookAndSay "1" `shouldBe` "11"

      it "11 becomes 21" $
        lookAndSay "11" `shouldBe` "21"

      it "21 becomes 1211" $
        lookAndSay "21" `shouldBe` "1211"

      it "1211 becomes 111221" $
        lookAndSay "1211" `shouldBe` "111221"

      it "111221 becomes 312211" $
        lookAndSay "111221" `shouldBe` "312211"

