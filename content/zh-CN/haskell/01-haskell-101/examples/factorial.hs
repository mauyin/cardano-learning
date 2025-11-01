{-
  Haskell 中的递归与模式匹配

  本示例演示：
  - 递归（函数调用自身）
  - 模式匹配（为不同输入定义不同行为）
  - 基础情况与递归情况
  - FP 如何用递归替代循环
-}

-- 类型签名：接受一个 Integer，返回一个 Integer
factorial :: Integer -> Integer

-- 模式匹配：我们定义两种不同的行为
-- 第一个模式：当输入为 0 时，返回 1（基础情况）
factorial 0 = 1

-- 第二个模式：对于任何其他数字 n（递归情况）
-- 计算 n * factorial(n-1)
factorial n = n * factorial (n - 1)

-- 让我们追踪 factorial 3 的计算过程：
-- factorial 3
-- = 3 * factorial 2
-- = 3 * (2 * factorial 1)
-- = 3 * (2 * (1 * factorial 0))
-- = 3 * (2 * (1 * 1))          -- 触发基础情况！
-- = 3 * (2 * 1)
-- = 3 * 2
-- = 6

-- 另一个使用守卫的示例（条件模式匹配）
factorialWithGuards :: Integer -> Integer
factorialWithGuards n
    | n <= 0    = 1  -- 守卫：如果 n <= 0，返回 1
    | otherwise = n * factorialWithGuards (n - 1)

-- 斐波那契数列：另一个经典递归示例
fibonacci :: Integer -> Integer
fibonacci 0 = 0  -- 基础情况 1
fibonacci 1 = 1  -- 基础情况 2
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  -- 递归情况

main :: IO ()
main = do
    putStrLn "=== 递归与模式匹配 ==="
    putStrLn ""

    putStrLn "阶乘示例："
    putStrLn $ "factorial 0 = " ++ show (factorial 0)
    putStrLn $ "factorial 1 = " ++ show (factorial 1)
    putStrLn $ "factorial 3 = " ++ show (factorial 3)
    putStrLn $ "factorial 5 = " ++ show (factorial 5)
    putStrLn $ "factorial 10 = " ++ show (factorial 10)

    putStrLn ""
    putStrLn "关键概念："
    putStrLn "- 模式匹配：factorial 0 = 1 在输入恰好为 0 时匹配"
    putStrLn "- 递归情况：factorial n = n * factorial (n-1) 匹配所有其他数字"
    putStrLn "- Haskell 从上到下检查模式"
    putStrLn "- 基础情况防止无限递归"

    putStrLn ""
    putStrLn "斐波那契数列示例："
    putStrLn $ "fibonacci 0 = " ++ show (fibonacci 0)
    putStrLn $ "fibonacci 1 = " ++ show (fibonacci 1)
    putStrLn $ "fibonacci 5 = " ++ show (fibonacci 5)
    putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10)

    putStrLn ""
    putStrLn "注意：斐波那契有两个基础情况！"
    putStrLn "fibonacci 0 = 0"
    putStrLn "fibonacci 1 = 1"
    putStrLn "fibonacci n = fibonacci (n-1) + fibonacci (n-2)"

{-
  运行此示例：

  在 GHCi 中：
  1. :load factorial.hs
  2. main
  3. 或直接测试：
     - factorial 5
     - fibonacci 7
     - factorialWithGuards 4

  编译并运行：
  1. ghc factorial.hs
  2. ./factorial

  挑战：你能写一个递归函数来求列表的和吗？
  sumList :: [Integer] -> Integer
  sumList [] = 0
  sumList (x:xs) = x + sumList xs
-}
