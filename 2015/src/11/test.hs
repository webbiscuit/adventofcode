import Test.Tasty.Hspec
import Day11

main = hspec $
  describe "2015 Day 11" $ do

    context "Testing password increments" $ do
      it "xx becomes xy" $
        incrementPassword "xx" `shouldBe` "xy"

      it "xz becomes ya" $
        incrementPassword "xz" `shouldBe` "ya"

    context "Testing requirements" $ do
      it "hijklmmn meets 1st requirement, fails 2nd requirement" $ do
        requirement1 "hijklmmn" `shouldBe` True
        requirement2 "hijklmmn" `shouldBe` False

      it "abbceffg meets 3rd requirement, fails 1st requirement" $ do
        requirement3 "abbceffg" `shouldBe` True
        requirement1 "abbceffg" `shouldBe` False

      it "abbcegjk fails third requirement" $
        requirement3 "abbcegjk" `shouldBe` False

    context "Testing password increments with requirements" $ do
      it "abcdefgh becomes abcdffaa" $
        incrementPasswordWithRequirements "abcdefgh" `shouldBe` "abcdffaa"

      it "ghijklmn becomes ghjaabcc" $
        incrementPasswordWithRequirements "ghijklmn" `shouldBe` "ghjaabcc"