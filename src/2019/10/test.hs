import Test.Tasty.Hspec
import Day10

main = hspec $
  describe "2019 Day 10" $

    context "Find monitoring station max asteroid count" $ do
      it "Simple map 1 should be 8" $ do
        map <- readFile "map1.txt"

        -- findBestMonitoringLocation (parseMap map) `shouldBe` (3, 4)
        findMostAsteroids (parseMap map) `shouldBe` 8

      it "Simple map 2 should be 33" $ do
        map <- readFile "map2.txt"

        -- findBestMonitoringLocation (parseMap map) `shouldBe` (5, 8)
        findMostAsteroids (parseMap map) `shouldBe` 33

      it "Simple map 3 should be 35" $ do
        map <- readFile "map3.txt"

        -- findBestMonitoringLocation (parseMap map) `shouldBe` (1, 2)
        findMostAsteroids (parseMap map) `shouldBe` 35

      it "Simple map 4 should be 41" $ do
        map <- readFile "map4.txt"

        -- findBestMonitoringLocation (parseMap map) `shouldBe` (6, 3)
        findMostAsteroids (parseMap map) `shouldBe` 41

      it "Simple map 5 should be 210" $ do
        map <- readFile "map5.txt"

        -- findBestMonitoringLocation (parseMap map) `shouldBe` (11, 13)
        findMostAsteroids (parseMap map) `shouldBe` 210