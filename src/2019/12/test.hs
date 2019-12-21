import Test.Tasty.Hspec
import Day12

main = hspec $
  describe "2019 Day 12" $
    context "Examples" $ do
      it "Should calculate steps correctly example 1" $ do
        let input  = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
        let moons = parseMoons input
        let step10 = applySteps moons 10
        let energy = calculateTotalEnergy step10

        energy `shouldBe` 179

      it "Should calculate steps correctly example 2" $ do
        let input  = "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"
        let moons = parseMoons input
        let step10 = applySteps moons 100
        let energy = calculateTotalEnergy step10
  
        energy `shouldBe` 1940

        