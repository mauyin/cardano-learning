{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
模块：Challenges
描述：Haskell 第一课挑战题

本模块包含 7 个高级挑战题，用于深入应用第一课的概念。

难度：⭐⭐⭐ (高级)
建议时间：3-5 小时

这些挑战题需要：
- 综合运用多个概念
- 更复杂的算法思维
- 递归和高阶函数的灵活运用
- 性能和边界情况的考虑

准备好挑战了吗？Let's go! 💪
-}

module Challenges where


-- ========================================
-- 挑战 1：运行长度编码 (Run-Length Encoding)
-- ========================================

{- |
挑战：runLengthEncode

实现运行长度编码算法，用于数据压缩。
将连续重复的元素编码为 (元素, 重复次数) 的列表。

示例：
>>> runLengthEncode "aaabbbcc"
[('a',3),('b',3),('c',2)]

>>> runLengthEncode "aabccccaaa"
[('a',2),('b',1),('c',4),('a',3)]

>>> runLengthEncode []
[]

>>> runLengthEncode "abcd"
[('a',1),('b',1),('c',1),('d',1)]

要求：
- 处理空列表
- 正确处理单个元素
- 相同元素不连续时应分别计数

提示：
- 使用递归
- 考虑使用辅助函数
- 可以使用 span 或 takeWhile/dropWhile
- span :: (a -> Bool) -> [a] -> ([a], [a])
  span (== 'a') "aaabbb" => ("aaa", "bbb")
-}
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode list = error "runLengthEncode: 请实现这个函数"


{- |
挑战扩展：runLengthDecode

解码运行长度编码的数据。

示例：
>>> runLengthDecode [('a',3),('b',3),('c',2)]
"aaabbbcc"

>>> runLengthDecode []
""

提示：使用 replicate 函数
-}
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode encoded = error "runLengthDecode: 请实现这个函数"


-- ========================================
-- 挑战 2：回文检测器
-- ========================================

{- |
挑战：isPalindrome

判断一个列表是否是回文（正读和反读相同）。

示例：
>>> isPalindrome "racecar"
True

>>> isPalindrome "hello"
False

>>> isPalindrome [1,2,3,2,1]
True

>>> isPalindrome []
True

>>> isPalindrome "a"
True

要求：
- 处理空列表（空列表是回文）
- 处理单个元素
- 忽略大小写（对于字符串）- 这是额外挑战！

提示：
- 最简单的方法：比较列表和它的反转
- 高级方法：只遍历一半列表
-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = error "isPalindrome: 请实现这个函数"


{- |
挑战扩展：longestPalindrome

找出字符串中最长的回文子串。

示例：
>>> longestPalindrome "babad"
"bab"  -- 或 "aba"

>>> longestPalindrome "cbbd"
"bb"

这是一个真正的挑战！需要检查所有可能的子串。
-}
longestPalindrome :: String -> String
longestPalindrome str = error "longestPalindrome: 请实现这个函数（高级挑战）"


-- ========================================
-- 挑战 3：合并排序 (Merge Sort)
-- ========================================

{- |
挑战：mergeSort

实现合并排序算法。
这是一个经典的分治算法，时间复杂度 O(n log n)。

示例：
>>> mergeSort [3, 1, 4, 1, 5, 9, 2, 6]
[1,1,2,3,4,5,6,9]

>>> mergeSort "haskell"
"aehklls"

>>> mergeSort []
[]

>>> mergeSort [5]
[5]

算法步骤：
1. 如果列表为空或只有一个元素，已经排序
2. 将列表分成两半
3. 递归排序两半
4. 合并两个已排序的列表

提示：
- 先实现 merge 辅助函数
- merge 接收两个已排序列表，返回合并后的排序列表
- 使用 splitAt 或 take/drop 分割列表
-}

-- 辅助函数：合并两个已排序列表
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = error "merge: 请实现这个函数"

-- 主函数：合并排序
mergeSort :: Ord a => [a] -> [a]
mergeSort list = error "mergeSort: 请实现这个函数"


-- ========================================
-- 挑战 4：素数生成器
-- ========================================

{- |
挑战：primes

生成所有素数的无限列表（使用埃拉托斯特尼筛法）。

示例：
>>> take 10 primes
[2,3,5,7,11,13,17,19,23,29]

>>> take 5 (filter (> 100) primes)
[101,103,107,109,113]

埃拉托斯特尼筛法：
1. 从 2 开始
2. 2 是素数
3. 删除所有 2 的倍数
4. 下一个数（3）是素数
5. 删除所有 3 的倍数
6. 重复...

提示：
- 使用惰性求值的优势
- sieve 函数筛选掉倍数
- filter 可以用来过滤倍数
-}
primes :: [Integer]
primes = error "primes: 请实现这个函数"


{- |
挑战扩展：isPrime

快速判断一个数是否为素数。

示例：
>>> isPrime 17
True

>>> isPrime 18
False

>>> isPrime 2
True

提示：只需检查到 √n
-}
isPrime :: Integer -> Bool
isPrime n = error "isPrime: 请实现这个函数"


-- ========================================
-- 挑战 5：表达式求值器
-- ========================================

{- |
挑战：evaluateRPN

实现逆波兰表示法（RPN）计算器。

RPN 是一种数学表达式表示法，操作符在操作数之后。
示例："3 4 +" 表示 3 + 4

示例：
>>> evaluateRPN "3 4 +"
7.0

>>> evaluateRPN "3 4 + 2 *"
14.0
-- 解释: (3 + 4) * 2 = 14

>>> evaluateRPN "15 7 1 1 + - / 3 * 2 1 1 + + -"
5.0
-- 这是一个复杂的表达式！

支持的操作符：+, -, *, /

算法（使用栈）：
1. 从左到右读取 tokens
2. 如果是数字，压入栈
3. 如果是操作符，弹出两个数，计算，将结果压入栈
4. 最后栈中应该只剩一个数

提示：
- 使用 words 分割字符串
- 使用列表作为栈
- 使用递归或 foldl
- 需要判断 token 是数字还是操作符
-}
evaluateRPN :: String -> Double
evaluateRPN expression = error "evaluateRPN: 请实现这个函数"


-- ========================================
-- 挑战 6：帕斯卡三角形
-- ========================================

{- |
挑战：pascalTriangle

生成帕斯卡三角形的前 n 行。

帕斯卡三角形：
     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1

每个数是其上方两个数的和。

示例：
>>> pascalTriangle 5
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]

>>> pascalTriangle 1
[[1]]

>>> pascalTriangle 0
[]

要求：
- 生成前 n 行
- 每行是一个列表
- 返回列表的列表

提示：
- 第一行是 [1]
- 每一行可以从上一行计算得出
- 使用 zipWith 函数很有帮助
- 思考如何给上一行两边添加 0，然后相邻相加
  例如：[1,2,1] => [0,1,2,1,0]
       相邻相加 => [1,3,3,1]
-}
pascalTriangle :: Int -> [[Integer]]
pascalTriangle n = error "pascalTriangle: 请实现这个函数"


{- |
挑战扩展：pascalRow

直接计算帕斯卡三角形的第 n 行（不生成前面的行）。

示例：
>>> pascalRow 4
[1,4,6,4,1]

提示：使用组合数公式 C(n,k) = n! / (k! * (n-k)!)
或者迭代计算每个元素
-}
pascalRow :: Int -> [Integer]
pascalRow n = error "pascalRow: 请实现这个函数（高级挑战）"


-- ========================================
-- 挑战 7：最长公共子序列 (LCS)
-- ========================================

{- |
挑战：longestCommonSubsequence

找出两个列表的最长公共子序列。

注意：子序列不要求连续，但要保持相对顺序。

示例：
>>> longestCommonSubsequence "abcde" "ace"
"ace"

>>> longestCommonSubsequence "abc" "abc"
"abc"

>>> longestCommonSubsequence "abc" "def"
""

>>> longestCommonSubsequence [1,2,3,4] [2,4,6]
[2,4]

这是一个经典的动态规划问题！

算法：
- 如果两个序列的第一个元素相同，它一定在 LCS 中
- 否则，LCS 是以下两者中较长的一个：
  - 第一个序列去掉第一个元素后，与第二个序列的 LCS
  - 第一个序列与第二个序列去掉第一个元素后的 LCS

提示：
- 使用递归
- 基础情况：任一列表为空
- 这个简单实现会很慢，优化是另一个挑战！
-}
longestCommonSubsequence :: Eq a => [a] -> [a] -> [a]
longestCommonSubsequence xs ys = error "longestCommonSubsequence: 请实现这个函数"


-- ========================================
-- 🏆 恭喜挑战挑战题！
-- ========================================

{- |
太厉害了！

如果你完成了这些挑战，你已经：
✓ 掌握了复杂的递归模式
✓ 理解了经典算法的实现
✓ 学会了分治和动态规划的基础
✓ 能够处理无限列表和惰性求值
✓ 具备了解决实际问题的能力

你现在已经：
- 理解了 Haskell 的核心概念
- 能够编写非平凡的函数
- 准备好学习更高级的主题了！

下一步：
1. 优化这些算法的性能
2. 学习 Lecture 2: Data Types
3. 探索 Haskell 的类型系统
4. 了解 Monad 和其他高级概念

你已经踏上了函数式编程的精彩旅程！

Keep learning, keep coding! 🎉🚀

"In Haskell, we don't tell the computer what to do,
 we tell it what things are."
-}
