import Test.Tasty.Hspec
import Day07
import Data.Word

main = hspec $
  describe "2015 Day 7" $

    context "Doing Logic" $ do
      it "Assigns with 123 -> x" $
        readSignal (createCircuit "123 -> x") "x" `shouldBe` 123

      it "ANDs x AND y -> d" $
        readSignal (createCircuit "123 -> x\n456 -> y\nx AND y -> d") "d" `shouldBe` 72

      it "ORs x OR y -> d" $
        readSignal (createCircuit "123 -> x\n456 -> y\nx OR y -> d") "d" `shouldBe` 507

      it "x LSHIFT 2 -> d" $
        readSignal (createCircuit "123 -> x\nx LSHIFT 2 -> d") "d"`shouldBe` 492

      it "x RSHIFT 2 -> d" $
        readSignal (createCircuit "123 -> x\nx RSHIFT 2 -> d") "d" `shouldBe` 30

      it "NOT x -> d" $
        readSignal (createCircuit "123 -> x\nNOT x -> d") "d" `shouldBe` 65412

      it "1 AND x -> d" $
        readSignal (createCircuit "123 -> x\n1 AND x -> d") "d" `shouldBe` 1

      it "x -> d" $
        readSignal (createCircuit "123 -> x\nx -> d") "d" `shouldBe` 123
