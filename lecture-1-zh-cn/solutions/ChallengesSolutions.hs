{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
模块：ChallengesSolutions
描述：挑战题参考答案

这是 Challenges.hs 的参考答案。
每个解答都包含详细的算法解释。
-}

module ChallengesSolutions where

-- 挑战 1：运行长度编码
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode (x:xs) = (x, length group) : runLengthEncode rest
  where
    (group, rest) = span (== x) (x:xs)

-- 解释：
-- span (== x) (x:xs) 将列表分成两部分：
--   group: 所有开头等于 x 的元素
--   rest: 剩余的元素
-- 然后递归处理剩余部分

runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode [] = []
runLengthDecode ((char, count):rest) = replicate count char ++ runLengthDecode rest

-- 或者使用 concatMap
runLengthDecode' :: [(a, Int)] -> [a]
runLengthDecode' = concatMap (\(char, count) -> replicate count char)


-- 挑战 2：回文检测
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

-- 优化版本：只比较一半
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' list = firstHalf == reverse secondHalf
  where
    len = length list
    firstHalf = take (len `div` 2) list
    secondHalf = drop ((len + 1) `div` 2) list


-- 挑战扩展：最长回文子串（简化版本）
longestPalindrome :: String -> String
longestPalindrome str = maximumBy compareLength allSubstrings
  where
    allSubstrings = [substring | i <- [0..length str - 1],
                                  j <- [i..length str - 1],
                                  let substring = take (j - i + 1) (drop i str),
                                  isPalindrome substring]
    compareLength a b = compare (length a) (length b)
    maximumBy _ [] = ""
    maximumBy cmp (x:xs) = foldl (\a b -> if cmp a b == LT then b else a) x xs


-- 挑战 3：合并排序
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length list `div` 2) list

-- 解释：
-- 1. 基础情况：空列表或单元素列表已排序
-- 2. 递归情况：
--    - 将列表分成两半
--    - 递归排序两半
--    - 合并两个已排序的列表


-- 挑战 4：素数生成器
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []  -- 这行永远不会执行，但为了完整性

-- 解释：
-- 埃拉托斯特尼筛法
-- 1. 取第一个数 p（素数）
-- 2. 过滤掉所有 p 的倍数
-- 3. 递归处理剩余的数

isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | otherwise = all (\x -> n `mod` x /= 0) [2..limit]
  where
    limit = floor (sqrt (fromIntegral n))

-- 更高效的版本
isPrime' :: Integer -> Bool
isPrime' n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = all (\x -> n `mod` x /= 0) [3,5..limit]
  where
    limit = floor (sqrt (fromIntegral n))


-- 挑战 5：RPN 计算器
evaluateRPN :: String -> Double
evaluateRPN = head . foldl processToken [] . words
  where
    processToken (x:y:rest) "+" = (y + x) : rest
    processToken (x:y:rest) "-" = (y - x) : rest
    processToken (x:y:rest) "*" = (y * x) : rest
    processToken (x:y:rest) "/" = (y / x) : rest
    processToken stack numStr   = read numStr : stack

-- 解释：
-- 1. words 将表达式分割成 tokens
-- 2. foldl 从左到右处理每个 token
-- 3. 数字压入栈，操作符弹出两个数计算后压回
-- 4. 最后栈顶就是结果

-- 递归版本
evaluateRPN' :: String -> Double
evaluateRPN' expression = head (evaluate (words expression) [])
  where
    evaluate [] stack = stack
    evaluate (token:rest) stack
        | token == "+" = evaluate rest (applyOp (+) stack)
        | token == "-" = evaluate rest (applyOp (-) stack)
        | token == "*" = evaluate rest (applyOp (*) stack)
        | token == "/" = evaluate rest (applyOp (/) stack)
        | otherwise    = evaluate rest (read token : stack)

    applyOp op (x:y:rest) = (y `op` x) : rest
    applyOp _ stack = stack


-- 挑战 6：帕斯卡三角形
pascalTriangle :: Int -> [[Integer]]
pascalTriangle 0 = []
pascalTriangle n = pascalTriangle (n - 1) ++ [pascalRow (n - 1)]

-- 或者更高效的版本
pascalTriangle' :: Int -> [[Integer]]
pascalTriangle' n = take n (iterate nextRow [1])
  where
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-- 解释：
-- [0,1,2,1,0]
-- [1,2,1,0]
-- 相加得到 [1,3,3,1]

pascalRow :: Int -> [Integer]
pascalRow 0 = [1]
pascalRow n = zipWith (+) ([0] ++ prevRow) (prevRow ++ [0])
  where
    prevRow = pascalRow (n - 1)

-- 使用组合数公式的版本
pascalRow' :: Int -> [Integer]
pascalRow' n = [choose n k | k <- [0..n]]
  where
    choose n k = factorial n `div` (factorial k * factorial (n - k))
    factorial 0 = 1
    factorial m = product [1..m]

-- 更高效的迭代版本
pascalRow'' :: Int -> [Integer]
pascalRow'' n = scanl (\acc k -> acc * (n - k + 1) `div` k) 1 [1..n]


-- 挑战 7：最长公共子序列
longestCommonSubsequence :: Eq a => [a] -> [a] -> [a]
longestCommonSubsequence [] _ = []
longestCommonSubsequence _ [] = []
longestCommonSubsequence (x:xs) (y:ys)
    | x == y    = x : longestCommonSubsequence xs ys
    | otherwise = longerOf (longestCommonSubsequence (x:xs) ys)
                           (longestCommonSubsequence xs (y:ys))
  where
    longerOf a b = if length a > length b then a else b

-- 解释：
-- 1. 如果第一个元素相同，它一定在 LCS 中
-- 2. 否则，LCS 是以下两者中较长的：
--    - 去掉第一个序列的第一个元素后的 LCS
--    - 去掉第二个序列的第一个元素后的 LCS

-- 注意：这个实现很慢（指数时间复杂度）
-- 实际应用中应该使用动态规划优化


{- |
算法复杂度分析：

1. runLengthEncode: O(n)
2. isPalindrome: O(n)
3. mergeSort: O(n log n)
4. primes (生成前 n 个): O(n² log log n) 大约
5. evaluateRPN: O(n)
6. pascalTriangle: O(n²)
7. longestCommonSubsequence (简单版): O(2^(m+n)) - 非常慢！
   动态规划版本: O(m*n) - 需要使用数组或 Map

学习要点：
- 递归思维
- 分治算法
- 惰性求值的应用
- 高阶函数的威力
- 算法复杂度的考虑
-}
