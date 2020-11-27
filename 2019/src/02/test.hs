import Test.Tasty.Hspec
import Day02

import Data.Vector

main = hspec $
  describe "2019 Day 2" $

    context "Restore Intcode program" $ do
      it "1,0,0,0,99 becomes 2,0,0,0,99" $
        runProgram (fromList [1,0,0,0,99]) `shouldBe` fromList [2,0,0,0,99]
      
      it "2,3,0,3,99 becomes 2,3,0,6,99" $
        runProgram (fromList [2,3,0,3,99]) `shouldBe` fromList [2,3,0,6,99]

      it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" $
        runProgram (fromList [2,4,4,5,99,0]) `shouldBe` fromList [2,4,4,5,99,9801]

      it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $
        runProgram (fromList [1,1,1,4,99,5,6,0,99]) `shouldBe` fromList [30,1,1,4,2,5,6,0,99]