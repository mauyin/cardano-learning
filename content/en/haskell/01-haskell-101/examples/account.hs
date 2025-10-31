{-
  Bank Account with Maybe Type - Error Handling in Haskell

  This example demonstrates:
  - Type aliases for clarity
  - The Maybe type for handling potential failures
  - Guards for conditional logic
  - Pattern matching on Maybe values
  - How Haskell forces you to handle errors explicitly
-}

-- Type alias: makes our code more readable
type Balance = Integer

-- Deposit always succeeds, so it returns a plain Balance
deposit :: Balance -> Integer -> Balance
deposit currentBalance amount = currentBalance + amount

-- Withdraw can fail (insufficient funds), so it returns Maybe Balance
-- Maybe has two constructors:
--   Just value  = success, contains the result
--   Nothing     = failure, no valid result
withdraw :: Balance -> Integer -> Maybe Balance
withdraw currentBalance amount
    | amount <= 0              = Nothing  -- Can't withdraw negative or zero
    | amount <= currentBalance = Just (currentBalance - amount)  -- Success!
    | otherwise                = Nothing  -- Insufficient funds

-- Transfer between accounts
-- Returns Nothing if withdrawal fails, otherwise returns new balances
transfer :: Balance -> Balance -> Integer -> Maybe (Balance, Balance)
transfer fromBalance toBalance amount =
    case withdraw fromBalance amount of
        Nothing -> Nothing  -- Withdrawal failed
        Just newFromBalance -> Just (newFromBalance, toBalance + amount)

-- Get balance or default value
getBalanceOrDefault :: Maybe Balance -> Balance -> Balance
getBalanceOrDefault maybeBalance defaultValue =
    case maybeBalance of
        Nothing      -> defaultValue
        Just balance -> balance

-- Chain multiple operations using Maybe
-- This demonstrates how Maybe prevents operations on invalid states
performOperations :: Balance -> Maybe Balance
performOperations balance = do
    -- In Haskell, 'do' notation allows us to chain Maybe operations
    balance1 <- withdraw balance 30     -- If this fails, whole chain fails
    balance2 <- withdraw balance1 20    -- This only runs if previous succeeded
    return (deposit balance2 10)        -- Final result

-- Another way: using case for explicit control
performOperationsExplicit :: Balance -> Maybe Balance
performOperationsExplicit balance =
    case withdraw balance 30 of
        Nothing -> Nothing
        Just balance1 ->
            case withdraw balance1 20 of
                Nothing -> Nothing
                Just balance2 -> Just (deposit balance2 10)

-- Helper function to print Maybe values nicely
showBalance :: Maybe Balance -> String
showBalance Nothing = "FAILED (insufficient funds)"
showBalance (Just b) = "SUCCESS: " ++ show b

main :: IO ()
main = do
    putStrLn "=== Bank Account with Maybe Type ==="
    putStrLn ""

    let startBalance = 100 :: Balance
    putStrLn $ "Starting balance: " ++ show startBalance

    putStrLn ""
    putStrLn "--- Deposit Operations (always succeed) ---"
    let afterDeposit = deposit startBalance 50
    putStrLn $ "After depositing 50: " ++ show afterDeposit

    putStrLn ""
    putStrLn "--- Withdrawal Operations (can fail) ---"

    let withdrawal1 = withdraw afterDeposit 30
    putStrLn $ "Withdraw 30: " ++ showBalance withdrawal1

    let withdrawal2 = withdraw afterDeposit 200
    putStrLn $ "Withdraw 200: " ++ showBalance withdrawal2

    let withdrawal3 = withdraw afterDeposit 150
    putStrLn $ "Withdraw 150: " ++ showBalance withdrawal3

    putStrLn ""
    putStrLn "--- Pattern Matching on Maybe ---"
    case withdrawal1 of
        Nothing -> putStrLn "First withdrawal failed!"
        Just newBalance -> putStrLn $ "First withdrawal succeeded! New balance: " ++ show newBalance

    case withdrawal2 of
        Nothing -> putStrLn "Second withdrawal failed! (as expected - insufficient funds)"
        Just newBalance -> putStrLn $ "Second withdrawal succeeded! New balance: " ++ show newBalance

    putStrLn ""
    putStrLn "--- Transfer Between Accounts ---"
    let account1 = 100
    let account2 = 50
    putStrLn $ "Account 1: " ++ show account1
    putStrLn $ "Account 2: " ++ show account2

    case transfer account1 account2 30 of
        Nothing -> putStrLn "Transfer failed!"
        Just (newAcc1, newAcc2) -> do
            putStrLn $ "Transfer succeeded!"
            putStrLn $ "Account 1 new balance: " ++ show newAcc1
            putStrLn $ "Account 2 new balance: " ++ show newAcc2

    case transfer account1 account2 150 of
        Nothing -> putStrLn "Transfer of 150 failed! (insufficient funds)"
        Just (newAcc1, newAcc2) -> do
            putStrLn $ "Transfer succeeded!"
            putStrLn $ "Account 1: " ++ show newAcc1
            putStrLn $ "Account 2: " ++ show newAcc2

    putStrLn ""
    putStrLn "--- Chained Operations ---"
    putStrLn "Starting with 100, withdraw 30, then 20, then deposit 10:"
    let result = performOperations 100
    putStrLn $ "Result: " ++ showBalance result

    putStrLn ""
    putStrLn "Starting with 50, withdraw 30, then 20, then deposit 10:"
    let result2 = performOperations 50
    putStrLn $ "Result: " ++ showBalance result2
    putStrLn "(Failed because after withdrawing 30 from 50, we only have 20 left, can't withdraw 20 more!)"

    putStrLn ""
    putStrLn "Key Insights:"
    putStrLn "- Maybe type forces you to handle both success and failure"
    putStrLn "- Just value = operation succeeded"
    putStrLn "- Nothing = operation failed"
    putStrLn "- Pattern matching ensures you can't ignore errors"
    putStrLn "- This prevents null pointer exceptions and undefined behavior"
    putStrLn "- Compared to exceptions, Maybe makes errors explicit in the type signature"

{-
  To run this example:

  In GHCi:
  1. :load account.hs
  2. main
  3. Or test directly:
     - withdraw 100 30
     - withdraw 100 150
     - transfer 100 50 30
     - performOperations 100

  To compile and run:
  1. ghc account.hs
  2. ./account

  The Maybe Type in Depth:

  data Maybe a = Nothing | Just a

  This is Haskell's way of saying "this operation might not return a value".
  Instead of throwing exceptions or returning null (which causes crashes),
  we explicitly wrap the result in Maybe.

  Benefits:
  1. Type system tells you which operations can fail
  2. Compiler forces you to handle both cases
  3. No null pointer exceptions
  4. No forgotten error handling
  5. Composable: you can chain Maybe operations

  This is crucial for blockchain/financial code where errors must be handled!
-}
