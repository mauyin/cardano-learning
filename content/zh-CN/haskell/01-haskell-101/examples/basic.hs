{-
  Haskell 中的基本函数与不可变性

  本示例演示：
  - 类型签名
  - 纯函数
  - 不可变性
  - 值不会改变，只会创建新值
-}

-- 类型签名：此函数接受两个 Integer 并返回一个 Integer
-- 将 "Integer -> Integer -> Integer" 理解为：
-- "接受一个 Integer，然后接受另一个 Integer，然后返回一个 Integer"
deposit :: Integer -> Integer -> Integer
deposit balance amount = balance + amount

-- 另一个示例：简单算术运算
add :: Integer -> Integer -> Integer
add x y = x + y

multiply :: Integer -> Integer -> Integer
multiply x y = x * y

-- 演示不可变性
main :: IO ()
main = do
    putStrLn "=== 基本函数与不可变性 ==="
    putStrLn ""

    -- 定义一个不可变的值
    let originalBalance = 100
    putStrLn $ "原始余额: " ++ show originalBalance

    -- 通过应用函数创建一个新值
    let newBalance = deposit originalBalance 50
    putStrLn $ "存入 50 后: " ++ show newBalance

    -- 注意：originalBalance 没有改变！
    putStrLn $ "原始余额（未改变）: " ++ show originalBalance

    putStrLn ""
    putStrLn "关键概念："
    putStrLn "- originalBalance 仍然是 100"
    putStrLn "- newBalance 是 150"
    putStrLn "- 我们创建了一个新值，而不是修改旧值"
    putStrLn "- 这就是不可变性 - 函数式编程安全性的基石"

    putStrLn ""
    putStrLn "=== 其他示例 ==="
    putStrLn $ "5 + 3 = " ++ show (add 5 3)
    putStrLn $ "5 * 3 = " ++ show (multiply 5 3)

{-
  运行此示例：

  在 GHCi（Haskell REPL）中：
  1. 加载文件：:load basic.hs
  2. 运行 main：main
  3. 或直接测试函数：
     - deposit 100 50
     - add 5 3
     - multiply 4 7

  编译并运行：
  1. ghc basic.hs
  2. ./basic
-}
