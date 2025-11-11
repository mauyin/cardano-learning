{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
模块：BasicDrills
描述：Haskell 第一课基础练习

本模块包含 18 个简单的练习，帮助你熟悉 Haskell 的基本语法和概念。

难度：⭐ (入门级)
建议时间：1-2 小时

主题：
- 算术运算
- 布尔逻辑
- 列表操作
- 函数定义
- 类型
-}

module BasicDrills where


-- ========================================
-- 第一部分：算术运算
-- ========================================

{- |
练习 1：double

任务：将一个整数乘以 2。

示例：
>>> double 5
10

>>> double (-3)
-6
-}
double :: Int -> Int
double x = error "double: 请实现这个函数"


{- |
练习 2：triple

任务：将一个整数乘以 3。

示例：
>>> triple 4
12

>>> triple 0
0
-}
triple :: Int -> Int
triple x = error "triple: 请实现这个函数"


{- |
练习 3：square

任务：计算一个数的平方。

示例：
>>> square 5
25

>>> square (-3)
9
-}
square :: Int -> Int
square x = error "square: 请实现这个函数"


{- |
练习 4：average

任务：计算两个数的平均值。

示例：
>>> average 10 20
15.0

>>> average 5 6
5.5

提示：
- 注意返回类型是 Double
- 使用 fromIntegral 将 Int 转换为 Double
-}
average :: Int -> Int -> Double
average x y = error "average: 请实现这个函数"


-- ========================================
-- 第二部分：布尔逻辑
-- ========================================

{- |
练习 5：isEven

任务：判断一个整数是否为偶数。

示例：
>>> isEven 4
True

>>> isEven 7
False

提示：使用 `mod` 函数
-}
isEven :: Int -> Bool
isEven n = error "isEven: 请实现这个函数"


{- |
练习 6：isOdd

任务：判断一个整数是否为奇数。

示例：
>>> isOdd 3
True

>>> isOdd 8
False

提示：可以基于 isEven 实现，或直接使用 mod
-}
isOdd :: Int -> Bool
isOdd n = error "isOdd: 请实现这个函数"


{- |
练习 7：isPositive

任务：判断一个整数是否为正数（大于 0）。

示例：
>>> isPositive 5
True

>>> isPositive 0
False

>>> isPositive (-3)
False
-}
isPositive :: Int -> Bool
isPositive n = error "isPositive: 请实现这个函数"


{- |
练习 8：isBetween

任务：判断一个数是否在给定范围内（包含边界）。

参数顺序：isBetween low high value

示例：
>>> isBetween 1 10 5
True

>>> isBetween 1 10 15
False

>>> isBetween 1 10 1
True

提示：使用 && 运算符组合两个条件
-}
isBetween :: Int -> Int -> Int -> Bool
isBetween low high value = error "isBetween: 请实现这个函数"


-- ========================================
-- 第三部分：列表操作
-- ========================================

{- |
练习 9：firstElement

任务：返回列表的第一个元素。

示例：
>>> firstElement [1, 2, 3]
1

>>> firstElement "hello"
'h'

提示：使用 head 函数
注意：此函数对空列表不安全！
-}
firstElement :: [a] -> a
firstElement list = error "firstElement: 请实现这个函数"


{- |
练习 10：secondElement

任务：返回列表的第二个元素。

示例：
>>> secondElement [1, 2, 3]
2

>>> secondElement "hello"
'e'

提示：可以使用 head 和 tail，或者使用 !! 运算符
-}
secondElement :: [a] -> a
secondElement list = error "secondElement: 请实现这个函数"


{- |
练习 11：listLength

任务：返回列表的长度。

示例：
>>> listLength [1, 2, 3, 4]
4

>>> listLength []
0

>>> listLength "hello"
5

提示：使用 length 函数
-}
listLength :: [a] -> Int
listLength list = error "listLength: 请实现这个函数"


{- |
练习 12：reverseList

任务：反转一个列表。

示例：
>>> reverseList [1, 2, 3]
[3,2,1]

>>> reverseList "hello"
"olleh"

提示：使用 reverse 函数
-}
reverseList :: [a] -> [a]
reverseList list = error "reverseList: 请实现这个函数"


{- |
练习 13：firstThree

任务：返回列表的前三个元素。

示例：
>>> firstThree [1, 2, 3, 4, 5]
[1,2,3]

>>> firstThree "hello"
"hel"

提示：使用 take 函数
-}
firstThree :: [a] -> [a]
firstThree list = error "firstThree: 请实现这个函数"


-- ========================================
-- 第四部分：字符串操作
-- ========================================

{- |
练习 14：greet

任务：给定一个名字，返回问候语 "Hello, [name]!"

示例：
>>> greet "Alice"
"Hello, Alice!"

>>> greet "Bob"
"Hello, Bob!"

提示：使用 ++ 连接字符串
-}
greet :: String -> String
greet name = error "greet: 请实现这个函数"


{- |
练习 15：shout

任务：将字符串转换为大写（我们假装 Haskell 有这个功能）。
实际上，请在字符串末尾添加三个感叹号。

示例：
>>> shout "hello"
"hello!!!"

>>> shout "watch out"
"watch out!!!"
-}
shout :: String -> String
shout str = error "shout: 请实现这个函数"


-- ========================================
-- 第五部分：条件逻辑
-- ========================================

{- |
练习 16：absoluteValue

任务：计算一个整数的绝对值（不使用内置的 abs 函数）。

示例：
>>> absoluteValue 5
5

>>> absoluteValue (-5)
5

>>> absoluteValue 0
0

提示：使用 if-then-else 表达式
-}
absoluteValue :: Int -> Int
absoluteValue n = error "absoluteValue: 请实现这个函数"


{- |
练习 17：maxOfTwo

任务：返回两个整数中的较大者（不使用内置的 max 函数）。

示例：
>>> maxOfTwo 5 10
10

>>> maxOfTwo 20 15
20

>>> maxOfTwo 7 7
7

提示：使用 if-then-else 或 guards
-}
maxOfTwo :: Int -> Int -> Int
maxOfTwo x y = error "maxOfTwo: 请实现这个函数"


{- |
练习 18：describeNumber

任务：根据数字返回描述：
- 如果是 0，返回 "zero"
- 如果是正数，返回 "positive"
- 如果是负数，返回 "negative"

示例：
>>> describeNumber 0
"zero"

>>> describeNumber 5
"positive"

>>> describeNumber (-3)
"negative"

提示：使用 guards 或 if-then-else
-}
describeNumber :: Int -> String
describeNumber n = error "describeNumber: 请实现这个函数"


-- ========================================
-- 恭喜完成基础练习！
-- ========================================

{- |
做得好！

通过完成这些基础练习，你已经学会了：
✓ 基本算术运算
✓ 布尔逻辑和条件判断
✓ 列表和字符串的基本操作
✓ 使用 if-then-else 和 guards
✓ 函数定义和类型签名

下一步：
1. 在 GHCi 中测试你的函数
2. 运行 `stack test` 验证答案
3. 继续挑战 Lecture1.hs 中的核心练习

继续努力！Keep coding! 💪
-}
