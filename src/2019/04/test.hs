import Test.Tasty.Hspec
import Day04

main = hspec $
  describe "2019 Day 4" $ do

    context "Checks for valid passwords" $ do
      it "111111 is valid" $
        isValidPassword 111111 `shouldBe` True
      
      it "223450 is not valid" $
        isValidPassword 223450 `shouldBe` False

      it "123789 is not valid" $
        isValidPassword 123789 `shouldBe` False

    context "Checks for valid v2 passwords" $ do
      it "112233 is valid" $
        isValidPasswordV2 112233 `shouldBe` True
      
      it "123444 is not valid" $
        isValidPasswordV2 123444 `shouldBe` False

      it "111122 is not valid" $
        isValidPasswordV2 111122 `shouldBe` True