import Test.Tasty.Hspec
import Day03

main = hspec $
  describe "2019 Day 3" $

    context "Intersection closest distance" $ do
      it "R8,U5,L5,D3 and U7,R6,D4,L4 intersects with distance 6" $
        calculateClosestIntersection (parseWire "R8,U5,L5,D3") (parseWire "U7,R6,D4,L4") `shouldBe` 6
      
      it "R75,D30,R83,U83,L12,D49,R71,U7,L72 and U62,R66,U55,R34,D71,R55,D58,R83 intersects with distance 159" $
        calculateClosestIntersection (parseWire "R75,D30,R83,U83,L12,D49,R71,U7,L72") (parseWire "U62,R66,U55,R34,D71,R55,D58,R83") `shouldBe` 159

      it "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 and U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 intersects with distance 135" $
        calculateClosestIntersection (parseWire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51") (parseWire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") `shouldBe` 135
