import Test.Tasty.Hspec
import Day06

main = hspec $
  describe "2019 Day 6" $

    context "Example" $ do
      it "Has a total orbit of 1" $
        countOrbits [
         ("COM","B")] `shouldBe` 1

      it "Has a total orbit of 42" $
        countOrbits [
          ("COM","B"),
          ("B","C"),
          ("C","D"),
          ("D","E"),
          ("E","F"),
          ("B","G"),
          ("G","H"),
          ("D","I"),
          ("E","J"),
          ("J","K"),
          ("K","L")] `shouldBe` 42

      it "Distance from YOU to SAN is 4" $
        countTransfers [
          ("COM","B"),
          ("B","C"),
          ("C","D"),
          ("D","E"),
          ("E","F"),
          ("B","G"),
          ("G","H"),
          ("D","I"),
          ("E","J"),
          ("J","K"),
          ("K","L"),
          ("K","YOU"),
          ("I","SAN")] "YOU" "SAN" `shouldBe` 4