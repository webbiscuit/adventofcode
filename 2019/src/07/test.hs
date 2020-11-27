import Test.Tasty.Hspec
import IntCodeComputer
import Day07

main = hspec $
  describe "2019 Day 7" $ do
    context "Example thruster programs" $ do
      it "Sequence 4,3,2,1,0 has max thruster signal 43210" $
        calculateThrusterSignal [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0] `shouldBe` 43210

      it "Sequence 0,1,2,3,4 has max thruster signal 54321" $
        calculateThrusterSignal [
          3,23,3,24,1002,24,10,24,1002,23,-1,23,
          101,5,23,23,1,24,23,23,4,23,99,0,0
        ] [0,1,2,3,4] `shouldBe` 54321

      it "Sequence 1,0,4,3,2 has max thruster signal 65210" $
        calculateThrusterSignal [
          3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
          1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
        ] [1,0,4,3,2] `shouldBe` 65210

    context "Example feedback thruster programs" $ do
      it "Sequence 9,8,7,6,5 has max thruster signal 139629729" $
        calculateFeedbackThrusterSignal [
          3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
          27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
        ] [9,8,7,6,5] `shouldBe` 139629729
    
      it "Sequence 9,7,8,5,6 has max thruster signal 18216" $
        calculateFeedbackThrusterSignal [
          3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
          -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
          53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
        ] [9,7,8,5,6] `shouldBe` 18216