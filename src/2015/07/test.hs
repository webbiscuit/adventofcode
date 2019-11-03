import Test.Tasty.Hspec
import Day06

main = hspec $
  describe "2015 Day 7" $

    context "Doing Logic" $
      it "123 -> x" $ do
        state <- execute "123 -> x"
        read state "x" `shouldBe` 123