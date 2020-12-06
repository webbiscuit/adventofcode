import Test.Tasty.Hspec
import Test.Hspec.Tables
import Day06

main = hspec $
  describe "2020 Day 6" $ do

    context "Can find unique yeses in group" $
      byExample
        ("group answers", "result")
        [ 
          (["abc"], "abc"),
          (["a","b","c"], "abc"),
          (["ab","bc"], "abc"),
          (["a","a","a","a"], "a"),
          (["b"], "b")
        ]
        (\a expected -> groupUniqueAnswers a `shouldBe` expected)

    context "Can count yeses" $
      it "Passes anyone example" $
        countAnyoneYeses [["abc"], ["a","b","c"], ["ab","bc"], ["a","a","a","a"], ["b"]] `shouldBe` 11

    context "Can find agreed yeses in group" $
      byExample
        ("group answers", "result")
        [ 
          (["abc"], "abc"),
          (["a","b","c"], ""),
          (["ab","ac"], "a"),
          (["a","a","a","a"], "a"),
          (["b"], "b")
        ]
        (\a expected -> groupUniqueAnswersForEveryone a `shouldBe` expected)

    context "Can count agreed yeses" $
      it "Passes everyone example" $
        countEveryoneYeses [["abc"], ["a","b","c"], ["ab","bc"], ["a","a","a","a"], ["b"]] `shouldBe` 6