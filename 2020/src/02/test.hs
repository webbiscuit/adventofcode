import Test.Tasty.Hspec
import Day02

main = hspec $
  describe "2020 Day 2" $ do

    context "Finding valid passwords" $ do
      it "1-3 a: abcde is valid" $
        isValidPassword (1, 3, 'a', "abcde") `shouldBe` True

      it "1-3 b: cdefg is invalid" $
        isValidPassword (1, 3, 'b', "cdefg") `shouldBe` False

      it "2-9 c: ccccccccc is valid" $
        isValidPassword (2, 9, 'c', "ccccccccc") `shouldBe` True

      it "Blank strings are invalid" $
        isValidPassword (1, 3, 'a', "")`shouldBe` False

    context "Password parsing" $ do
      it "1-3 a: abcde is parsed" $
        parsePasswordPolicies "1-3 a: abcde is parsed" `shouldBe` [(1, 3, 'a', "abcde")]

      it "1-3 b: cdefg is parsed" $
        parsePasswordPolicies "1-3 b: cdefg is parsed" `shouldBe` [(1, 3, 'b', "cdefg")]

      it "2-9 c: ccccccccc is parsed" $
        parsePasswordPolicies "2-9 c: ccccccccc is parsed" `shouldBe` [(2, 9, 'c', "ccccccccc")]

    context "Finding valid v2 passwords" $ do
      it "1-3 a: abcde is valid" $
        isValidPasswordV2 (1, 3, 'a', "abcde") `shouldBe` True

      it "1-3 b: cdefg is invalid" $
        isValidPasswordV2 (1, 3, 'b', "cdefg") `shouldBe` False

      it "2-9 c: ccccccccc is valid" $
        isValidPasswordV2 (2, 9, 'c', "ccccccccc") `shouldBe` False
      
