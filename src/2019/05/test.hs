import Test.Tasty.Hspec
import Day05

import Data.Vector

main = hspec $
  describe "2019 Day 5" $ do

    context "Handles immediate mode" $
      it "1002,4,3,4,33 becomes 1002,4,3,4,99" $
        runProgram (fromList [1002,4,3,4,33]) 0 `shouldBe` (fromList [1002,4,3,4,99],[])

    context "Handles input and outputs" $
      it "3,0,4,0,99 becomes 10,0,4,0,99" $
        runProgram (fromList [3,0,4,0,99]) 10 `shouldBe` (fromList [10,0,4,0,99],[10])

    context "Handles immediate outputs" $
      it "104,0,99 becomes 104,0,99" $
        runProgram (fromList [104,0,99]) 0 `shouldBe` (fromList [104,0,99],[0])

    context "Handles negatives" $
      it "1101,100,-1,4,0 becomes 1101,100,-1,4,99" $
        runProgram (fromList [1101,100,-1,4,0]) 10 `shouldBe` (fromList [1101,100,-1,4,99],[])

    context "Considers input equal to 8" $ do
      it "3,9,8,9,10,9,4,9,99,-1,8 and input 8 outputs 1" $
        runProgram (fromList [3,9,8,9,10,9,4,9,99,-1,8]) 8 `shouldBe` (fromList [3,9,8,9,10,9,4,9,99,1,8],[1])

      it "3,9,8,9,10,9,4,9,99,-1,8 and input 7 outputs 0" $
        runProgram (fromList [3,9,8,9,10,9,4,9,99,-1,8]) 7 `shouldBe` (fromList [3,9,8,9,10,9,4,9,99,0,8],[0])

    context "Considers input less than 8" $ do
      it "3,9,7,9,10,9,4,9,99,-1,8 and input 7 outputs 1" $
        runProgram (fromList [3,9,7,9,10,9,4,9,99,-1,8]) 7 `shouldBe` (fromList [3,9,7,9,10,9,4,9,99,1,8],[1])

      it "3,9,8,9,10,9,4,9,99,-1,8 and input 8 outputs 0" $
        runProgram (fromList [3,9,7,9,10,9,4,9,99,-1,8]) 8 `shouldBe` (fromList [3,9,7,9,10,9,4,9,99,0,8],[0])        
        
    context "Considers input equal to 8 (immediate mode)" $ do
      it "3,3,1108,-1,8,3,4,3,99 and input 8 outputs 1" $
        runProgram (fromList [3,3,1108,-1,8,3,4,3,99]) 8 `shouldBe` (fromList [3,3,1108,1,8,3,4,3,99],[1])
    
      it "3,3,1108,-1,8,3,4,3,99 and input 7 outputs 0" $
        runProgram (fromList [3,3,1108,-1,8,3,4,3,99]) 7 `shouldBe` (fromList [3,3,1108,0,8,3,4,3,99],[0]) 

    context "Considers input less than 8 (immediate mode)" $ do
      it "3,3,1107,-1,8,3,4,3,99 and input 7 outputs 1" $
        runProgram (fromList [3,3,1107,-1,8,3,4,3,99]) 7 `shouldBe` (fromList [3,3,1107,1,8,3,4,3,99],[1])
        
      it "3,3,1107,-1,8,3,4,3,99 and input 8 outputs 0" $
        runProgram (fromList [3,3,1107,-1,8,3,4,3,99]) 8 `shouldBe` (fromList [3,3,1107,0,8,3,4,3,99],[0]) 

    context "Jump tests" $ do
      it "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 and input 0 outputs 0" $
        runProgram (fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) 0 `shouldBe` (fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9],[0])
            
      it "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 and input 1 outputs 1" $
        runProgram (fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) 1 `shouldBe` (fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9],[1]) 

    context "Jump tests (immediate mode)" $ do
      it "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 and input 0 outputs 0" $
        runProgram (fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) 0 `shouldBe` (fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1],[0])
                
      it "3,3,1105,-1,9,1101,0,0,12,4,12,99,1 and input 1 outputs 1" $
        runProgram (fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) 1 `shouldBe` (fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1],[1]) 
    
    context "Run day 2 tests" $ do
      it "1,0,0,0,99 becomes 2,0,0,0,99" $
        runProgram (fromList [1,0,0,0,99]) 0 `shouldBe` (fromList [2,0,0,0,99],[])
      
      it "2,3,0,3,99 becomes 2,3,0,6,99" $
        runProgram (fromList [2,3,0,3,99]) 0 `shouldBe` (fromList [2,3,0,6,99],[])

      it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
        runProgram (fromList [2,4,4,5,99,0]) 0 `shouldBe` (fromList [2,4,4,5,99,9801],[])

      it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
        runProgram (fromList [1,1,1,4,99,5,6,0,99]) 0 `shouldBe` (fromList [30,1,1,4,2,5,6,0,99],[])