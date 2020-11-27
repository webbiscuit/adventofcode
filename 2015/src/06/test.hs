import Test.Tasty.Hspec
import Day06

main = hspec $
  describe "2015 Day 6" $ do

    context "Counting lights" $ do
      it "turn on 0,0 through 999,999" $
        countLightsOn "turn on 0,0 through 999,999" `shouldBe` 1000000
      it "toggle 0,0 through 999,999" $
        countLightsOn "toggle 0,0 through 999,999" `shouldBe` 1000000
      it "turn off 1,1 through 2,2" $
        countLightsOn "turn on 0,0 through 9,9\nturn off 1,1 through 2,2" `shouldBe` (100 - 4)

    context "Measure brightness" $ do
      it "turn on 0,0 through 0,0" $
        measureBrightness "turn on 0,0 through 0,0" `shouldBe` 1
      it "toggle 0,0 through 999,999" $
        measureBrightness "toggle 0,0 through 999,999" `shouldBe` 2000000