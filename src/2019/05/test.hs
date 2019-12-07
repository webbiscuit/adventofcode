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

    context "Run day 2 tests" $ do
      it "1,0,0,0,99 becomes 2,0,0,0,99" $
        runProgram (fromList [1,0,0,0,99]) 0 `shouldBe` (fromList [2,0,0,0,99],[])
      
      it "2,3,0,3,99 becomes 2,3,0,6,99" $
        runProgram (fromList [2,3,0,3,99]) 0 `shouldBe` (fromList [2,3,0,6,99],[])

      it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
        runProgram (fromList [2,4,4,5,99,0]) 0 `shouldBe` (fromList [2,4,4,5,99,9801],[])

      it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
        runProgram (fromList [1,1,1,4,99,5,6,0,99]) 0 `shouldBe` (fromList [30,1,1,4,2,5,6,0,99],[])