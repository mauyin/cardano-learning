module ChallengesSpec (spec) where

import Test.Hspec
import Challenges

spec :: Spec
spec = do
  describe "Challenges - 挑战题测试" $ do

    describe "runLengthEncode - 运行长度编码" $ do
      it "runLengthEncode \"aaabbbcc\" 应该返回 [('a',3),('b',3),('c',2)]" $
        runLengthEncode "aaabbbcc" `shouldBe` [('a', 3), ('b', 3), ('c', 2)]

      it "runLengthEncode \"aabccccaaa\" 应该返回 [('a',2),('b',1),('c',4),('a',3)]" $
        runLengthEncode "aabccccaaa" `shouldBe` [('a', 2), ('b', 1), ('c', 4), ('a', 3)]

      it "runLengthEncode [] 应该返回 []" $
        runLengthEncode ([] :: [Char]) `shouldBe` []

      it "runLengthEncode \"abcd\" 应该返回 [('a',1),('b',1),('c',1),('d',1)]" $
        runLengthEncode "abcd" `shouldBe` [('a', 1), ('b', 1), ('c', 1), ('d', 1)]

    describe "runLengthDecode - 运行长度解码" $ do
      it "runLengthDecode [('a',3),('b',3),('c',2)] 应该返回 \"aaabbbcc\"" $
        runLengthDecode [('a', 3), ('b', 3), ('c', 2)] `shouldBe` "aaabbbcc"

      it "runLengthDecode [] 应该返回 \"\"" $
        runLengthDecode ([] :: [(Char, Int)]) `shouldBe` ""

    describe "isPalindrome - 回文检测" $ do
      it "isPalindrome \"racecar\" 应该返回 True" $
        isPalindrome "racecar" `shouldBe` True

      it "isPalindrome \"hello\" 应该返回 False" $
        isPalindrome "hello" `shouldBe` False

      it "isPalindrome [1,2,3,2,1] 应该返回 True" $
        isPalindrome [1, 2, 3, 2, 1] `shouldBe` True

      it "isPalindrome [] 应该返回 True" $
        isPalindrome ([] :: [Int]) `shouldBe` True

      it "isPalindrome \"a\" 应该返回 True" $
        isPalindrome "a" `shouldBe` True

    describe "merge - 合并已排序列表" $ do
      it "merge [1,3,5] [2,4,6] 应该返回 [1,2,3,4,5,6]" $
        merge [1, 3, 5] [2, 4, 6] `shouldBe` [1, 2, 3, 4, 5, 6]

      it "merge [] [1,2,3] 应该返回 [1,2,3]" $
        merge [] [1, 2, 3] `shouldBe` [1, 2, 3]

      it "merge [1,2,3] [] 应该返回 [1,2,3]" $
        merge [1, 2, 3] [] `shouldBe` [1, 2, 3]

    describe "mergeSort - 合并排序" $ do
      it "mergeSort [3,1,4,1,5,9,2,6] 应该排序正确" $
        mergeSort [3, 1, 4, 1, 5, 9, 2, 6] `shouldBe` [1, 1, 2, 3, 4, 5, 6, 9]

      it "mergeSort \"haskell\" 应该返回 \"aehklls\"" $
        mergeSort "haskell" `shouldBe` "aehklls"

      it "mergeSort [] 应该返回 []" $
        mergeSort ([] :: [Int]) `shouldBe` []

      it "mergeSort [5] 应该返回 [5]" $
        mergeSort [5] `shouldBe` [5]

    describe "primes - 素数生成" $ do
      it "前10个素数应该正确" $
        take 10 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

      it "大于100的前5个素数应该正确" $
        take 5 (filter (> 100) primes) `shouldBe` [101, 103, 107, 109, 113]

    describe "isPrime - 素数判断" $ do
      it "isPrime 17 应该返回 True" $
        isPrime 17 `shouldBe` True

      it "isPrime 18 应该返回 False" $
        isPrime 18 `shouldBe` False

      it "isPrime 2 应该返回 True" $
        isPrime 2 `shouldBe` True

      it "isPrime 1 应该返回 False" $
        isPrime 1 `shouldBe` False

    describe "evaluateRPN - RPN计算器" $ do
      it "evaluateRPN \"3 4 +\" 应该返回 7.0" $
        evaluateRPN "3 4 +" `shouldBe` 7.0

      it "evaluateRPN \"3 4 + 2 *\" 应该返回 14.0" $
        evaluateRPN "3 4 + 2 *" `shouldBe` 14.0

      it "evaluateRPN \"15 7 1 1 + - / 3 * 2 1 1 + + -\" 应该返回 5.0" $
        evaluateRPN "15 7 1 1 + - / 3 * 2 1 1 + + -" `shouldBe` 5.0

    describe "pascalTriangle - 帕斯卡三角形" $ do
      it "pascalTriangle 5 应该返回正确的三角形" $
        pascalTriangle 5 `shouldBe` [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]

      it "pascalTriangle 1 应该返回 [[1]]" $
        pascalTriangle 1 `shouldBe` [[1]]

      it "pascalTriangle 0 应该返回 []" $
        pascalTriangle 0 `shouldBe` []

    describe "pascalRow - 帕斯卡三角形特定行" $ do
      it "pascalRow 4 应该返回 [1,4,6,4,1]" $
        pascalRow 4 `shouldBe` [1, 4, 6, 4, 1]

      it "pascalRow 0 应该返回 [1]" $
        pascalRow 0 `shouldBe` [1]

    describe "longestCommonSubsequence - 最长公共子序列" $ do
      it "longestCommonSubsequence \"abcde\" \"ace\" 应该返回 \"ace\"" $
        longestCommonSubsequence "abcde" "ace" `shouldBe` "ace"

      it "longestCommonSubsequence \"abc\" \"abc\" 应该返回 \"abc\"" $
        longestCommonSubsequence "abc" "abc" `shouldBe` "abc"

      it "longestCommonSubsequence \"abc\" \"def\" 应该返回 \"\"" $
        longestCommonSubsequence "abc" "def" `shouldBe` ""

      it "longestCommonSubsequence [1,2,3,4] [2,4,6] 应该返回 [2,4]" $
        longestCommonSubsequence [1, 2, 3, 4] [2, 4, 6] `shouldBe` [2, 4]
