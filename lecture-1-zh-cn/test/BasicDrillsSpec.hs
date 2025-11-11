module BasicDrillsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import BasicDrills

spec :: Spec
spec = do
  describe "BasicDrills - 基础练习测试" $ do

    -- 算术运算测试
    describe "double - 乘以2" $ do
      it "double 5 应该返回 10" $
        double 5 `shouldBe` 10

      it "double 0 应该返回 0" $
        double 0 `shouldBe` 0

      it "double (-3) 应该返回 -6" $
        double (-3) `shouldBe` (-6)

    describe "triple - 乘以3" $ do
      it "triple 4 应该返回 12" $
        triple 4 `shouldBe` 12

      it "triple 0 应该返回 0" $
        triple 0 `shouldBe` 0

    describe "square - 平方" $ do
      it "square 5 应该返回 25" $
        square 5 `shouldBe` 25

      it "square (-3) 应该返回 9" $
        square (-3) `shouldBe` 9

      it "square 0 应该返回 0" $
        square 0 `shouldBe` 0

    describe "average - 平均值" $ do
      it "average 10 20 应该返回 15.0" $
        average 10 20 `shouldBe` 15.0

      it "average 5 6 应该接近 5.5" $
        average 5 6 `shouldSatisfy` (\x -> abs (x - 5.5) < 0.01)

    -- 布尔逻辑测试
    describe "isEven - 判断偶数" $ do
      it "isEven 4 应该返回 True" $
        isEven 4 `shouldBe` True

      it "isEven 7 应该返回 False" $
        isEven 7 `shouldBe` False

      it "isEven 0 应该返回 True" $
        isEven 0 `shouldBe` True

    describe "isOdd - 判断奇数" $ do
      it "isOdd 3 应该返回 True" $
        isOdd 3 `shouldBe` True

      it "isOdd 8 应该返回 False" $
        isOdd 8 `shouldBe` False

    describe "isPositive - 判断正数" $ do
      it "isPositive 5 应该返回 True" $
        isPositive 5 `shouldBe` True

      it "isPositive 0 应该返回 False" $
        isPositive 0 `shouldBe` False

      it "isPositive (-3) 应该返回 False" $
        isPositive (-3) `shouldBe` False

    describe "isBetween - 判断范围" $ do
      it "isBetween 1 10 5 应该返回 True" $
        isBetween 1 10 5 `shouldBe` True

      it "isBetween 1 10 15 应该返回 False" $
        isBetween 1 10 15 `shouldBe` False

      it "isBetween 1 10 1 应该返回 True (包含下界)" $
        isBetween 1 10 1 `shouldBe` True

      it "isBetween 1 10 10 应该返回 True (包含上界)" $
        isBetween 1 10 10 `shouldBe` True

    -- 列表操作测试
    describe "firstElement - 第一个元素" $ do
      it "firstElement [1,2,3] 应该返回 1" $
        firstElement [1, 2, 3] `shouldBe` 1

      it "firstElement \"hello\" 应该返回 'h'" $
        firstElement "hello" `shouldBe` 'h'

    describe "secondElement - 第二个元素" $ do
      it "secondElement [1,2,3] 应该返回 2" $
        secondElement [1, 2, 3] `shouldBe` 2

      it "secondElement \"hello\" 应该返回 'e'" $
        secondElement "hello" `shouldBe` 'e'

    describe "listLength - 列表长度" $ do
      it "listLength [1,2,3,4] 应该返回 4" $
        listLength [1, 2, 3, 4] `shouldBe` 4

      it "listLength [] 应该返回 0" $
        listLength [] `shouldBe` 0

      it "listLength \"hello\" 应该返回 5" $
        listLength "hello" `shouldBe` 5

    describe "reverseList - 反转列表" $ do
      it "reverseList [1,2,3] 应该返回 [3,2,1]" $
        reverseList [1, 2, 3] `shouldBe` [3, 2, 1]

      it "reverseList \"hello\" 应该返回 \"olleh\"" $
        reverseList "hello" `shouldBe` "olleh"

      it "reverseList [] 应该返回 []" $
        reverseList ([] :: [Int]) `shouldBe` []

    describe "firstThree - 前三个元素" $ do
      it "firstThree [1,2,3,4,5] 应该返回 [1,2,3]" $
        firstThree [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3]

      it "firstThree \"hello\" 应该返回 \"hel\"" $
        firstThree "hello" `shouldBe` "hel"

    -- 字符串操作测试
    describe "greet - 问候" $ do
      it "greet \"Alice\" 应该返回 \"Hello, Alice!\"" $
        greet "Alice" `shouldBe` "Hello, Alice!"

      it "greet \"Bob\" 应该返回 \"Hello, Bob!\"" $
        greet "Bob" `shouldBe` "Hello, Bob!"

    describe "shout - 喊叫" $ do
      it "shout \"hello\" 应该返回 \"hello!!!\"" $
        shout "hello" `shouldBe` "hello!!!"

      it "shout \"watch out\" 应该返回 \"watch out!!!\"" $
        shout "watch out" `shouldBe` "watch out!!!"

    -- 条件逻辑测试
    describe "absoluteValue - 绝对值" $ do
      it "absoluteValue 5 应该返回 5" $
        absoluteValue 5 `shouldBe` 5

      it "absoluteValue (-5) 应该返回 5" $
        absoluteValue (-5) `shouldBe` 5

      it "absoluteValue 0 应该返回 0" $
        absoluteValue 0 `shouldBe` 0

    describe "maxOfTwo - 最大值" $ do
      it "maxOfTwo 5 10 应该返回 10" $
        maxOfTwo 5 10 `shouldBe` 10

      it "maxOfTwo 20 15 应该返回 20" $
        maxOfTwo 20 15 `shouldBe` 20

      it "maxOfTwo 7 7 应该返回 7" $
        maxOfTwo 7 7 `shouldBe` 7

    describe "describeNumber - 描述数字" $ do
      it "describeNumber 0 应该返回 \"zero\"" $
        describeNumber 0 `shouldBe` "zero"

      it "describeNumber 5 应该返回 \"positive\"" $
        describeNumber 5 `shouldBe` "positive"

      it "describeNumber (-3) 应该返回 \"negative\"" $
        describeNumber (-3) `shouldBe` "negative"
