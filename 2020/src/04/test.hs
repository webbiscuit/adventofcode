import Test.Tasty.Hspec
import Day04

main = hspec $
  describe "2020 Day 4" $ do

    context "Can validate passports" $ do
      it "Can validate simple passport" $
        isValidPassport (parsePasswordPolicies "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm") `shouldBe` True

      it "Can detect invalidate simple passport" $
        isValidPassport (parsePasswordPolicies "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929") `shouldBe` False

      it "Can validate simple passport without optional field" $
        isValidPassport (parsePasswordPolicies "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm") `shouldBe` True

      it "Can detect invalidate simple passport with missing fields" $
        isValidPassport (parsePasswordPolicies "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in") `shouldBe` False
 
    context "Can count valid passports" $
      it "Counts valid passports" $ do
        example <- readFile "example.txt"
        length (validPassports example) `shouldBe` 2

    context "Can count valid passports with extra validation" $ do
      it "Counts valid passports v2" $ do
        example <- readFile "example2-valid.txt"
        length (validPassports2 example) `shouldBe` 4

      it "Counts invalid passports v2" $ do
        example <- readFile "example2-invalid.txt"
        length (validPassports2 example) `shouldBe` 0

    context "Can validate fields" $ do
      it "Checks byr is valid" $
        isByrValid (Just "2002") `shouldBe` True
      it "Checks byr is invalid" $
        isByrValid (Just "2003") `shouldBe` False

      it "Checks hgt (in) is valid" $
        isHgtValid (Just "60in") `shouldBe` True
      it "Checks hgt (cm) is valid" $
        isHgtValid (Just "190cm") `shouldBe` True
      it "Checks hgt (in) is invalid" $
        isHgtValid (Just "190in") `shouldBe` False
      it "Checks hgt (cm) is invalid" $
        isHgtValid (Just "190") `shouldBe` False

      it "Checks hcl is valid" $
        isHclValid (Just "#123abc") `shouldBe` True
      it "Checks hcl is invalid" $
        isHclValid (Just "#123abz") `shouldBe` False
      it "Checks another hcl is invalid" $
        isHclValid (Just "123abc")`shouldBe` False
 
      it "Checks ecl is valid" $
        isEclValid (Just "brn") `shouldBe` True
      it "Checks ecl is invalid" $
        isEclValid (Just "wat") `shouldBe` False

      it "Checks pid is valid" $
        isPidValid (Just "000000001") `shouldBe` True
      it "Checks pid is invalid" $
        isPidValid (Just "0123456789") `shouldBe` False
