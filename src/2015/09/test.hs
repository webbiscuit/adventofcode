import Test.Tasty.Hspec
import Day09

main = hspec $
  describe "2015 Day 9" $

    context "Calculating Distances" $
      it "Calculate Dublin -> London -> Belfast = 982" $ do
        let locationMap = buildLocationMap [ LocationDistance "London" "Dublin" 464, LocationDistance "London" "Belfast" 518, LocationDistance "Dublin" "Belfast" 141]

        calculateRouteDistance locationMap ["Dublin", "London", "Belfast"] `shouldBe` 982

