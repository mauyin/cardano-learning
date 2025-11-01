{-
  Haskell 中的列表操作与高阶函数

  本示例演示：
  - 列表语法与操作
  - 高阶函数（map、filter、foldr 等）
  - Lambda 函数
  - 函数组合
  - 为什么列表在函数式编程中是基础
-}

-- 基本列表示例
numbers :: [Integer]
numbers = [1, 2, 3, 4, 5]

names :: [String]
names = ["Alice", "Bob", "Charlie"]

-- 使用 map：对每个元素应用函数
-- map :: (a -> b) -> [a] -> [b]
squaredNumbers :: [Integer]
squaredNumbers = map (\x -> x * x) numbers
-- 使用 lambda：\x -> x * x 表示"一个接受 x 并返回 x 平方的函数"

doubledNumbers :: [Integer]
doubledNumbers = map (*2) numbers
-- (*2) 是部分应用：创建一个乘以 2 的函数

-- 使用 filter：只保留满足条件的元素
-- filter :: (a -> Bool) -> [a] -> [a]
evenNumbers :: [Integer]
evenNumbers = filter even numbers

oddNumbers :: [Integer]
oddNumbers = filter odd numbers

greaterThanThree :: [Integer]
greaterThanThree = filter (>3) numbers

-- 使用 foldr：将列表归约为单个值
-- foldr :: (a -> b -> b) -> b -> [a] -> b
sumAll :: Integer
sumAll = foldr (+) 0 numbers
-- 这表示："使用 (+) 从右向左折叠，起始值为 0"
-- foldr (+) 0 [1,2,3,4,5]
-- = 1 + (2 + (3 + (4 + (5 + 0))))
-- = 15

productAll :: Integer
productAll = foldr (*) 1 numbers

-- 函数组合：组合多个操作
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
processNumbers :: [Integer] -> Integer
processNumbers = foldr (+) 0 . map (*2) . filter even
-- 从右向左阅读：
-- 1. filter even：保留偶数
-- 2. map (*2)：将每个数字翻倍
-- 3. foldr (+) 0：求和

-- 列表推导（类似 Python）
squaresComprehension :: [Integer]
squaresComprehension = [x * x | x <- [1..10]]

evenSquares :: [Integer]
evenSquares = [x * x | x <- [1..10], even x]

-- 实用示例：处理配对
pairs :: [(String, Integer)]
pairs = [("Alice", 25), ("Bob", 30), ("Charlie", 35)]

-- 提取名字
extractNames :: [(String, Integer)] -> [String]
extractNames = map fst  -- fst 获取配对的第一个元素

-- 按年龄筛选
olderThan30 :: [(String, Integer)] -> [(String, Integer)]
olderThan30 = filter (\(name, age) -> age > 30)

main :: IO ()
main = do
    putStrLn "=== 列表与高阶函数 ==="
    putStrLn ""

    putStrLn "原始列表："
    putStrLn $ "numbers = " ++ show numbers

    putStrLn ""
    putStrLn "使用 map（转换每个元素）："
    putStrLn $ "平方 = " ++ show squaredNumbers
    putStrLn $ "翻倍 = " ++ show doubledNumbers

    putStrLn ""
    putStrLn "使用 filter（保留满足条件的元素）："
    putStrLn $ "偶数 = " ++ show evenNumbers
    putStrLn $ "奇数 = " ++ show oddNumbers
    putStrLn $ "大于 3 = " ++ show greaterThanThree

    putStrLn ""
    putStrLn "使用 foldr（归约为单个值）："
    putStrLn $ "总和 = " ++ show sumAll
    putStrLn $ "乘积 = " ++ show productAll

    putStrLn ""
    putStrLn "函数组合："
    putStrLn $ "processNumbers [1..10] = " ++ show (processNumbers [1..10])
    putStrLn "  （筛选偶数，翻倍，然后求和）"

    putStrLn ""
    putStrLn "列表推导："
    putStrLn $ "1-10 的平方 = " ++ show squaresComprehension
    putStrLn $ "偶数的平方 = " ++ show evenSquares

    putStrLn ""
    putStrLn "处理配对："
    putStrLn $ "配对 = " ++ show pairs
    putStrLn $ "仅名字 = " ++ show (extractNames pairs)
    putStrLn $ "大于 30 岁 = " ++ show (olderThan30 pairs)

    putStrLn ""
    putStrLn "关键概念："
    putStrLn "- map：转换每个元素"
    putStrLn "- filter：保留满足条件的元素"
    putStrLn "- foldr：将列表归约为单个值"
    putStrLn "- Lambda：\\x -> x * 2 是一个匿名函数"
    putStrLn "- 组合：使用 (.) 组合函数"
    putStrLn "- 所有这些操作都创建新列表，从不修改原始列表！"

{-
  运行此示例：

  在 GHCi 中：
  1. :load lists.hs
  2. main
  3. 或直接测试：
     - map (*3) [1,2,3,4,5]
     - filter (>10) [5,10,15,20]
     - foldr (+) 0 [1,2,3,4,5]

  编译并运行：
  1. ghc lists.hs
  2. ./lists

  挑战：你能使用这些函数来：
  1. 求 1 到 100 所有奇数的和？
     foldr (+) 0 (filter odd [1..100])

  2. 获取长度超过 4 个字符的名字数量？
     length (filter (\name -> length name > 4) names)
-}
