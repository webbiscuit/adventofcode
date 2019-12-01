import Test.Tasty.Hspec
import Day01

main = hspec $
  describe "2019 Day 1" $ do

    context "Calculating fuel for mass" $ do
      it "Mass 12 needs 2 fuel" $
        calculateFuel 12 `shouldBe` 2
      
      it "Mass 14 needs 2 fuel" $
        calculateFuel 14 `shouldBe` 2

      it "Mass 1969 needs 654 fuel" $
        calculateFuel 1969 `shouldBe` 654

      it "Mass 100756 needs 33583 fuel" $
        calculateFuel 100756 `shouldBe` 33583

    context "Calculating fuel for mass when fuel has a mass" $ do
      it "Mass 14 + extra fuel needs 2 fuel" $
        calculateFuelWithFuelMass 14 `shouldBe` 2

      it "Mass 1969 + extra fuel needs 966 fuel" $
        calculateFuelWithFuelMass 1969 `shouldBe` 966

      it "Mass 100756 + extra fuel needs 50346 fuel" $
        calculateFuelWithFuelMass 100756 `shouldBe` 50346

      it "Mass 2 + extra fuel needs 0 fuel" $
        calculateFuelWithFuelMass 2 `shouldBe` 0
