import Test.Tasty.Hspec
import Day07

import Data.Vector

main = hspec $
  describe "2019 Day 7" $ do
    context "Example thruster programs" $ do
      it "Sequence 4,3,2,1,0 has max thruster signal 43210" $
        calculateThrusterSignal (fromList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) [4,3,2,1,0] `shouldBe` 43210

      it "Sequence 0,1,2,3,4 has max thruster signal 54321" $
        calculateThrusterSignal (fromList [
          3,23,3,24,1002,24,10,24,1002,23,-1,23,
          101,5,23,23,1,24,23,23,4,23,99,0,0
        ]) [0,1,2,3,4] `shouldBe` 54321

      it "Sequence 1,0,4,3,2 has max thruster signal 65210" $
        calculateThrusterSignal (fromList [
          3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
          1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
        ]) [1,0,4,3,2] `shouldBe` 65210

    context "Example feedback thruster programs" $ do
      it "Sequence 9,8,7,6,5 has max thruster signal 139629729" $
        calculateFeedbackThrusterSignal (fromList [
          3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
          27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
        ]) [9,8,7,6,5] `shouldBe` 139629729
    
      it "Sequence 9,7,8,5,6 has max thruster signal 18216" $
        calculateFeedbackThrusterSignal (fromList [
          3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
          -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
          53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
        ]) [9,7,8,5,6] `shouldBe` 18216

    context "Run Day 7 tests" $ do
      it "1002,4,3,4,33 becomes 1002,4,3,4,99" $
        runProgram (fromList [1002,4,3,4,33]) [0] `shouldBe` (fromList [1002,4,3,4,99],4,[])

      it "3,0,4,0,99 becomes 10,0,4,0,99" $
        runProgram (fromList [3,0,4,0,99]) [10] `shouldBe` (fromList [10,0,4,0,99],4,[10])

      it "104,0,99 becomes 104,0,99" $
        runProgram (fromList [104,0,99]) [0] `shouldBe` (fromList [104,0,99],2,[0])

      it "1101,100,-1,4,0 becomes 1101,100,-1,4,99" $
        runProgram (fromList [1101,100,-1,4,0]) [10] `shouldBe` (fromList [1101,100,-1,4,99],4,[])

      it "3,9,8,9,10,9,4,9,99,-1,8 and input 8 outputs 1" $
        runProgram (fromList [3,9,8,9,10,9,4,9,99,-1,8]) [8] `shouldBe` (fromList [3,9,8,9,10,9,4,9,99,1,8],8,[1])

      it "3,9,8,9,10,9,4,9,99,-1,8 and input 7 outputs 0" $
        runProgram (fromList [3,9,8,9,10,9,4,9,99,-1,8]) [7] `shouldBe` (fromList [3,9,8,9,10,9,4,9,99,0,8],8,[0])

      it "3,9,7,9,10,9,4,9,99,-1,8 and input 7 outputs 1" $
        runProgram (fromList [3,9,7,9,10,9,4,9,99,-1,8]) [7] `shouldBe` (fromList [3,9,7,9,10,9,4,9,99,1,8],8,[1])

      it "3,9,8,9,10,9,4,9,99,-1,8 and input 8 outputs 0" $
        runProgram (fromList [3,9,7,9,10,9,4,9,99,-1,8]) [8] `shouldBe` (fromList [3,9,7,9,10,9,4,9,99,0,8],8,[0])        
        
      it "3,3,1108,-1,8,3,4,3,99 and input 8 outputs 1" $
        runProgram (fromList [3,3,1108,-1,8,3,4,3,99]) [8] `shouldBe` (fromList [3,3,1108,1,8,3,4,3,99],8,[1])
    
      it "3,3,1108,-1,8,3,4,3,99 and input 7 outputs 0" $
        runProgram (fromList [3,3,1108,-1,8,3,4,3,99]) [7] `shouldBe` (fromList [3,3,1108,0,8,3,4,3,99],8,[0]) 

      it "3,3,1107,-1,8,3,4,3,99 and input 7 outputs 1" $
        runProgram (fromList [3,3,1107,-1,8,3,4,3,99]) [7] `shouldBe` (fromList [3,3,1107,1,8,3,4,3,99],8,[1])
        
      it "3,3,1107,-1,8,3,4,3,99 and input 8 outputs 0" $
        runProgram (fromList [3,3,1107,-1,8,3,4,3,99]) [8] `shouldBe` (fromList [3,3,1107,0,8,3,4,3,99],8,[0]) 

      it "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 and input 0 outputs 0" $
        runProgram (fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) [0] `shouldBe` (fromList [3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9],11,[0])
            
      it "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 and input 1 outputs 1" $
        runProgram (fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) [1] `shouldBe` (fromList [3,12,6,12,15,1,13,14,13,4,13,99,1,1,1,9],11,[1]) 

      it "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 and input 0 outputs 0" $
        runProgram (fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) [0] `shouldBe` (fromList [3,3,1105,0,9,1101,0,0,12,4,12,99,0],11,[0])
                
      it "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 and input 1 outputs 1" $
        runProgram (fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) [1] `shouldBe` (fromList [3,3,1105,1,9,1101,0,0,12,4,12,99,1],11,[1]) 
    
      it "Input 7 outputs 999" $
        runProgram (fromList [
          3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        ]) [7] `shouldBe` (fromList [
          3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,0,7,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        ],46,[999])
                    
      it "Input 8 outputs 1000" $
        runProgram (fromList [
          3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        ]) [8] `shouldBe` (fromList [
          3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,1000,8,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        ],46,[1000])
    
      it "Input 9 outputs 1001" $
        runProgram (fromList [
          3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        ]) [9] `shouldBe` (fromList [
          3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,1001,9,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
        ],46,[1001]) 
            
    context "Run day 2 tests" $ do
      it "1,0,0,0,99 becomes 2,0,0,0,99" $
        runProgram (fromList [1,0,0,0,99]) [0] `shouldBe` (fromList [2,0,0,0,99],4,[])
      
      it "2,3,0,3,99 becomes 2,3,0,6,99" $
        runProgram (fromList [2,3,0,3,99]) [0] `shouldBe` (fromList [2,3,0,6,99],4,[])

      it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
        runProgram (fromList [2,4,4,5,99,0]) [0] `shouldBe` (fromList [2,4,4,5,99,9801],4,[])

      it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
        runProgram (fromList [1,1,1,4,99,5,6,0,99]) [0] `shouldBe` (fromList [30,1,1,4,2,5,6,0,99],8,[])