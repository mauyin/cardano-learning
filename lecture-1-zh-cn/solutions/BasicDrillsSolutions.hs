{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
模块：BasicDrillsSolutions
描述：基础练习参考答案

这是 BasicDrills.hs 的参考答案。
如果你在练习中遇到困难，可以参考这些实现。

注意：这些只是参考答案，可能还有其他正确的实现方式！
-}

module BasicDrillsSolutions where

-- 算术运算
double :: Int -> Int
double x = x * 2

triple :: Int -> Int
triple x = x * 3

square :: Int -> Int
square x = x * x

average :: Int -> Int -> Double
average x y = fromIntegral (x + y) / 2.0

-- 布尔逻辑
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

isOdd :: Int -> Bool
isOdd n = n `mod` 2 /= 0
-- 或者: isOdd n = not (isEven n)

isPositive :: Int -> Bool
isPositive n = n > 0

isBetween :: Int -> Int -> Int -> Bool
isBetween low high value = value >= low && value <= high

-- 列表操作
firstElement :: [a] -> a
firstElement list = head list

secondElement :: [a] -> a
secondElement list = head (tail list)
-- 或者: secondElement list = list !! 1

listLength :: [a] -> Int
listLength list = length list

reverseList :: [a] -> [a]
reverseList list = reverse list

firstThree :: [a] -> [a]
firstThree list = take 3 list

-- 字符串操作
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

shout :: String -> String
shout str = str ++ "!!!"

-- 条件逻辑
absoluteValue :: Int -> Int
absoluteValue n = if n < 0 then -n else n

maxOfTwo :: Int -> Int -> Int
maxOfTwo x y = if x >= y then x else y
-- 或者使用 guards:
-- maxOfTwo x y
--   | x >= y    = x
--   | otherwise = y

describeNumber :: Int -> String
describeNumber n
    | n == 0    = "zero"
    | n > 0     = "positive"
    | otherwise = "negative"
-- 或者使用 if-then-else:
-- describeNumber n = if n == 0 then "zero"
--                    else if n > 0 then "positive"
--                    else "negative"
