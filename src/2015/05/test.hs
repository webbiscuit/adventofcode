import Test.Tasty.Hspec
import Day05

main = hspec $ do
  describe "2015 Day 5" $ do

    context "Checks for naughty strings" $ do
      it "ugknbfddgicrmopn" $ do
        isNiceString "ugknbfddgicrmopn" `shouldBe` True
      it "aaa" $ do
        isNiceString "aaa" `shouldBe` True
      it "jchzalrnumimnmhp" $ do
        isNiceString "jchzalrnumimnmhp" `shouldBe` False
      it "haegwjzuvuyypxyu" $ do
        isNiceString "haegwjzuvuyypxyu" `shouldBe` False
      it "dvszwmarrgswjxmb" $ do
        isNiceString "dvszwmarrgswjxmb" `shouldBe` False

    context "Checks for naughty strings v2" $ do
      it "qjhvhtzxzqqjkmpb" $ do
        isNiceString2 "qjhvhtzxzqqjkmpb" `shouldBe` True
      it "xxyxx" $ do
        isNiceString2 "xxyxx" `shouldBe` True
      it "uurcxstgmygtbstg" $ do
        isNiceString2 "uurcxstgmygtbstg" `shouldBe` False
      it "ieodomkazucvgmuy" $ do
        isNiceString2 "ieodomkazucvgmuy" `shouldBe` False
