{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
模块：Lecture1Solutions
描述：核心练习参考答案

这是 Lecture1.hs 的参考答案。
每个练习都提供了详细的解释和可能的替代实现。
-}

module Lecture1Solutions where

-- 练习 1：makeSnippet
-- 类型签名
makeSnippet :: Int -> String -> String
makeSnippet n str
  | length str <= n = str
  | otherwise       = take n str ++ "..."


-- 练习 2：sumOfSquares
sumOfSquares :: Integer -> Integer
sumOfSquares n = sum (map square [1..n])
  where
    square x = x * x

-- 替代实现 1：使用列表推导式
sumOfSquares' :: Integer -> Integer
sumOfSquares' n = sum [x * x | x <- [1..n]]

-- 替代实现 2：使用 (^) 运算符
sumOfSquares'' :: Integer -> Integer
sumOfSquares'' n = sum (map (^2) [1..n])


-- 练习 3：lastDigit
lastDigit :: Int -> Int
lastDigit n = abs n `mod` 10

-- 解释：
-- abs n 确保处理负数
-- `mod` 10 提取最后一位数字


-- 练习 4：minmax
minmax :: (Ord a, Num a) => a -> a -> a -> a
minmax a b c = maxVal - minVal
  where
    maxVal = max a (max b c)
    minVal = min a (min b c)

-- 替代实现：使用 maximum 和 minimum
minmax' :: (Ord a, Num a) => a -> a -> a -> a
minmax' a b c = maximum [a, b, c] - minimum [a, b, c]


-- 练习 5：subString
subString :: Int -> Int -> String -> String
subString start end str = take (end - start + 1) (drop start str)

-- 解释：
-- drop start str 丢弃前 start 个字符
-- take (end - start + 1) 取所需的字符数
-- +1 是因为 end 是包含的

-- 替代实现：处理边界情况
subString' :: Int -> Int -> String -> String
subString' start end str
    | start >= length str = ""
    | end >= length str   = drop start str
    | otherwise           = take (end - start + 1) (drop start str)


-- 练习 6：strSum
strSum :: String -> Int
strSum str
    | null (words str) = 0
    | otherwise        = sum (map read (words str))

-- 替代实现：更简洁
strSum' :: String -> Int
strSum' "" = 0
strSum' str = sum (map read (words str))

-- 解释：
-- words str 将字符串按空格分割成单词列表
-- map read 将每个单词字符串转换为整数
-- sum 求和


-- 练习 7：lowerAndGreater
lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater threshold list =
    "Lower: " ++ show lowerCount ++ "; Greater: " ++ show greaterCount
  where
    lowerCount = length (filter (< threshold) list)
    greaterCount = length (filter (> threshold) list)

-- 替代实现：使用一次遍历
lowerAndGreater' :: Int -> [Int] -> String
lowerAndGreater' threshold list =
    "Lower: " ++ show lower ++ "; Greater: " ++ show greater
  where
    (lower, greater) = countBoth list
    countBoth [] = (0, 0)
    countBoth (x:xs)
        | x < threshold = (1 + fst rest, snd rest)
        | x > threshold = (fst rest, 1 + snd rest)
        | otherwise     = rest
      where
        rest = countBoth xs

-- 替代实现：使用 foldr（高级）
lowerAndGreater'' :: Int -> [Int] -> String
lowerAndGreater'' threshold list =
    "Lower: " ++ show lower ++ "; Greater: " ++ show greater
  where
    (lower, greater) = foldr count (0, 0) list
    count x (l, g)
        | x < threshold = (l + 1, g)
        | x > threshold = (l, g + 1)
        | otherwise     = (l, g)


{- |
学习笔记：

练习 1 (makeSnippet):
- 学习了 guards 的使用
- 理解了字符串操作函数

练习 2 (sumOfSquares):
- map 函数的应用
- 列表推导式
- sum 函数

练习 3 (lastDigit):
- mod 运算符
- abs 函数处理负数

练习 4 (minmax):
- 嵌套的 max/min 调用
- where 子句的使用

练习 5 (subString):
- take 和 drop 的组合使用
- 索引计算

练习 6 (strSum):
- words 函数分割字符串
- read 函数类型转换
- map 和 sum 的组合

练习 7 (lowerAndGreater):
- filter 函数的应用
- length 函数
- 字符串拼接
- 单遍历优化（高级）
-}
