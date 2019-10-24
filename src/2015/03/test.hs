import Test.Tasty.Hspec
import Day03

main = hspec $ do
  describe "2015 Day 3" $ do

    context "Counting houses that get presents" $ do
      it "Passes the examples" $ do
        countHousesWithPresents ">" `shouldBe` 2
        countHousesWithPresents "^>v<" `shouldBe` 4
        countHousesWithPresents "^v^v^v^v^v" `shouldBe` 2
      
    context "Counting houses that get presents using a robot" $ do
      it "Passes the examples" $ do
        countHousesWithPresentsUsingRobot "^v" `shouldBe` 3
        countHousesWithPresentsUsingRobot "^>v<" `shouldBe` 3
        countHousesWithPresentsUsingRobot "^v^v^v^v^v" `shouldBe` 11
