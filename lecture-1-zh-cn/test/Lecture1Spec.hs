module Lecture1Spec (spec) where

import Test.Hspec
import Lecture1

spec :: Spec
spec = do
  describe "Lecture1 - 核心练习测试" $ do

    describe "makeSnippet - 文本截断" $ do
      it "makeSnippet 7 \"Hello, World!\" 应该返回 \"Hello, ...\"" $
        makeSnippet 7 "Hello, World!" `shouldBe` "Hello, ..."

      it "makeSnippet 20 \"Short\" 应该返回 \"Short\"" $
        makeSnippet 20 "Short" `shouldBe` "Short"

      it "makeSnippet 0 \"Any string\" 应该返回 \"...\"" $
        makeSnippet 0 "Any string" `shouldBe` "..."

      it "makeSnippet 5 \"Hello\" 应该返回 \"Hello\"" $
        makeSnippet 5 "Hello" `shouldBe` "Hello"

    describe "sumOfSquares - 平方和" $ do
      it "sumOfSquares 3 应该返回 14" $
        sumOfSquares 3 `shouldBe` 14

      it "sumOfSquares 5 应该返回 55" $
        sumOfSquares 5 `shouldBe` 55

      it "sumOfSquares 0 应该返回 0" $
        sumOfSquares 0 `shouldBe` 0

      it "sumOfSquares 1 应该返回 1" $
        sumOfSquares 1 `shouldBe` 1

      it "sumOfSquares 10 应该返回 385" $
        sumOfSquares 10 `shouldBe` 385

    describe "lastDigit - 最后一位数字" $ do
      it "lastDigit 123 应该返回 3" $
        lastDigit 123 `shouldBe` 3

      it "lastDigit 5 应该返回 5" $
        lastDigit 5 `shouldBe` 5

      it "lastDigit (-17) 应该返回 7" $
        lastDigit (-17) `shouldBe` 7

      it "lastDigit 0 应该返回 0" $
        lastDigit 0 `shouldBe` 0

      it "lastDigit 9999 应该返回 9" $
        lastDigit 9999 `shouldBe` 9

    describe "minmax - 最大最小值差" $ do
      it "minmax 7 1 4 应该返回 6" $
        minmax 7 1 4 `shouldBe` 6

      it "minmax 3 3 3 应该返回 0" $
        minmax 3 3 3 `shouldBe` 0

      it "minmax (-5) 0 10 应该返回 15" $
        minmax (-5) 0 10 `shouldBe` 15

      it "minmax 10 5 7 应该返回 5" $
        minmax 10 5 7 `shouldBe` 5

    describe "subString - 子字符串" $ do
      it "subString 2 5 \"Hello, World!\" 应该返回 \"llo,\"" $
        subString 2 5 "Hello, World!" `shouldBe` "llo,"

      it "subString 0 4 \"Haskell\" 应该返回 \"Haske\"" $
        subString 0 4 "Haskell" `shouldBe` "Haske"

      it "subString 7 20 \"Short\" 应该返回 \"\"" $
        subString 7 20 "Short" `shouldBe` ""

      it "subString 0 0 \"Test\" 应该返回 \"T\"" $
        subString 0 0 "Test" `shouldBe` "T"

    describe "strSum - 字符串求和" $ do
      it "strSum \"1 2 3\" 应该返回 6" $
        strSum "1 2 3" `shouldBe` 6

      it "strSum \"10 -5 3\" 应该返回 8" $
        strSum "10 -5 3" `shouldBe` 8

      it "strSum \"\" 应该返回 0" $
        strSum "" `shouldBe` 0

      it "strSum \"   \" 应该返回 0" $
        strSum "   " `shouldBe` 0

      it "strSum \"42\" 应该返回 42" $
        strSum "42" `shouldBe` 42

      it "strSum \"1 2 3 4 5\" 应该返回 15" $
        strSum "1 2 3 4 5" `shouldBe` 15

    describe "lowerAndGreater - 阈值统计" $ do
      it "lowerAndGreater 5 [1,3,5,7,9] 应该返回 \"Lower: 2; Greater: 2\"" $
        lowerAndGreater 5 [1, 3, 5, 7, 9] `shouldBe` "Lower: 2; Greater: 2"

      it "lowerAndGreater 0 [-5,0,5] 应该返回 \"Lower: 1; Greater: 1\"" $
        lowerAndGreater 0 [(-5), 0, 5] `shouldBe` "Lower: 1; Greater: 1"

      it "lowerAndGreater 10 [1,2,3] 应该返回 \"Lower: 3; Greater: 0\"" $
        lowerAndGreater 10 [1, 2, 3] `shouldBe` "Lower: 3; Greater: 0"

      it "lowerAndGreater 5 [] 应该返回 \"Lower: 0; Greater: 0\"" $
        lowerAndGreater 5 [] `shouldBe` "Lower: 0; Greater: 0"

      it "lowerAndGreater 5 [10,20,30] 应该返回 \"Lower: 0; Greater: 3\"" $
        lowerAndGreater 5 [10, 20, 30] `shouldBe` "Lower: 0; Greater: 3"
