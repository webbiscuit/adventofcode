import Test.Tasty.Hspec
import Day0X

main = hspec $
  describe "2020 Day X" $

    context "Some test" $ do
      it "Do thing" $
        test 1 `shouldBe` 2
      
      it "Do thing 2" $
        test 11 `shouldBe` 21
