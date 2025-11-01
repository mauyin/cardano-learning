{-
  使用 Maybe 类型的银行账户 - Haskell 中的错误处理

  本示例演示：
  - 使用类型别名提高代码清晰度
  - Maybe 类型处理潜在失败
  - 使用守卫进行条件逻辑
  - 对 Maybe 值进行模式匹配
  - Haskell 如何强制您显式处理错误
-}

-- 类型别名：使我们的代码更易读
type Balance = Integer

-- 存款总是成功，所以返回普通的 Balance
deposit :: Balance -> Integer -> Balance
deposit currentBalance amount = currentBalance + amount

-- 取款可能失败（余额不足），所以返回 Maybe Balance
-- Maybe 有两个构造器：
--   Just value  = 成功，包含结果
--   Nothing     = 失败，没有有效结果
withdraw :: Balance -> Integer -> Maybe Balance
withdraw currentBalance amount
    | amount <= 0              = Nothing  -- 不能取出负数或零
    | amount <= currentBalance = Just (currentBalance - amount)  -- 成功！
    | otherwise                = Nothing  -- 余额不足

-- 账户之间的转账
-- 如果取款失败返回 Nothing，否则返回新余额
transfer :: Balance -> Balance -> Integer -> Maybe (Balance, Balance)
transfer fromBalance toBalance amount =
    case withdraw fromBalance amount of
        Nothing -> Nothing  -- 取款失败
        Just newFromBalance -> Just (newFromBalance, toBalance + amount)

-- 获取余额或默认值
getBalanceOrDefault :: Maybe Balance -> Balance -> Balance
getBalanceOrDefault maybeBalance defaultValue =
    case maybeBalance of
        Nothing      -> defaultValue
        Just balance -> balance

-- 使用 Maybe 链接多个操作
-- 这演示了 Maybe 如何防止对无效状态进行操作
performOperations :: Balance -> Maybe Balance
performOperations balance = do
    -- 在 Haskell 中，'do' 语法允许我们链接 Maybe 操作
    balance1 <- withdraw balance 30     -- 如果失败，整个链条失败
    balance2 <- withdraw balance1 20    -- 只有前一个成功才运行
    return (deposit balance2 10)        -- 最终结果

-- 另一种方式：使用 case 进行显式控制
performOperationsExplicit :: Balance -> Maybe Balance
performOperationsExplicit balance =
    case withdraw balance 30 of
        Nothing -> Nothing
        Just balance1 ->
            case withdraw balance1 20 of
                Nothing -> Nothing
                Just balance2 -> Just (deposit balance2 10)

-- 辅助函数，用于美观地打印 Maybe 值
showBalance :: Maybe Balance -> String
showBalance Nothing = "失败（余额不足）"
showBalance (Just b) = "成功：" ++ show b

main :: IO ()
main = do
    putStrLn "=== 使用 Maybe 类型的银行账户 ==="
    putStrLn ""

    let startBalance = 100 :: Balance
    putStrLn $ "起始余额：" ++ show startBalance

    putStrLn ""
    putStrLn "--- 存款操作（总是成功）---"
    let afterDeposit = deposit startBalance 50
    putStrLn $ "存入 50 后：" ++ show afterDeposit

    putStrLn ""
    putStrLn "--- 取款操作（可能失败）---"

    let withdrawal1 = withdraw afterDeposit 30
    putStrLn $ "取款 30：" ++ showBalance withdrawal1

    let withdrawal2 = withdraw afterDeposit 200
    putStrLn $ "取款 200：" ++ showBalance withdrawal2

    let withdrawal3 = withdraw afterDeposit 150
    putStrLn $ "取款 150：" ++ showBalance withdrawal3

    putStrLn ""
    putStrLn "--- 对 Maybe 进行模式匹配 ---"
    case withdrawal1 of
        Nothing -> putStrLn "第一次取款失败！"
        Just newBalance -> putStrLn $ "第一次取款成功！新余额：" ++ show newBalance

    case withdrawal2 of
        Nothing -> putStrLn "第二次取款失败！（如预期 - 余额不足）"
        Just newBalance -> putStrLn $ "第二次取款成功！新余额：" ++ show newBalance

    putStrLn ""
    putStrLn "--- 账户之间转账 ---"
    let account1 = 100
    let account2 = 50
    putStrLn $ "账户 1：" ++ show account1
    putStrLn $ "账户 2：" ++ show account2

    case transfer account1 account2 30 of
        Nothing -> putStrLn "转账失败！"
        Just (newAcc1, newAcc2) -> do
            putStrLn $ "转账成功！"
            putStrLn $ "账户 1 新余额：" ++ show newAcc1
            putStrLn $ "账户 2 新余额：" ++ show newAcc2

    case transfer account1 account2 150 of
        Nothing -> putStrLn "转账 150 失败！（余额不足）"
        Just (newAcc1, newAcc2) -> do
            putStrLn $ "转账成功！"
            putStrLn $ "账户 1：" ++ show newAcc1
            putStrLn $ "账户 2：" ++ show newAcc2

    putStrLn ""
    putStrLn "--- 链式操作 ---"
    putStrLn "从 100 开始，取款 30，然后取款 20，然后存款 10："
    let result = performOperations 100
    putStrLn $ "结果：" ++ showBalance result

    putStrLn ""
    putStrLn "从 50 开始，取款 30，然后取款 20，然后存款 10："
    let result2 = performOperations 50
    putStrLn $ "结果：" ++ showBalance result2
    putStrLn "（失败是因为从 50 取款 30 后，只剩 20，无法再取款 20！）"

    putStrLn ""
    putStrLn "关键概念："
    putStrLn "- Maybe 类型强制您处理成功和失败两种情况"
    putStrLn "- Just value = 操作成功"
    putStrLn "- Nothing = 操作失败"
    putStrLn "- 模式匹配确保您无法忽略错误"
    putStrLn "- 这防止了空指针异常和未定义行为"
    putStrLn "- 与异常相比，Maybe 在类型签名中显式表示错误"

{-
  运行此示例：

  在 GHCi 中：
  1. :load account.hs
  2. main
  3. 或直接测试：
     - withdraw 100 30
     - withdraw 100 150
     - transfer 100 50 30
     - performOperations 100

  编译并运行：
  1. ghc account.hs
  2. ./account

  深入理解 Maybe 类型：

  data Maybe a = Nothing | Just a

  这是 Haskell 表达"此操作可能不返回值"的方式。
  与抛出异常或返回 null（导致崩溃）不同，
  我们显式地将结果包装在 Maybe 中。

  好处：
  1. 类型系统告诉您哪些操作可能失败
  2. 编译器强制您处理两种情况
  3. 没有空指针异常
  4. 不会忘记错误处理
  5. 可组合：您可以链接 Maybe 操作

  这对于必须处理错误的区块链/金融代码至关重要！
-}
