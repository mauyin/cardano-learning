{-
  Basic Functions and Immutability in Haskell

  This example demonstrates:
  - Type signatures
  - Pure functions
  - Immutability
  - How values don't change, only new values are created
-}

-- Type signature: This function takes two Integers and returns an Integer
-- Read "Integer -> Integer -> Integer" as:
-- "takes an Integer, then takes another Integer, then returns an Integer"
deposit :: Integer -> Integer -> Integer
deposit balance amount = balance + amount

-- Another example: Simple arithmetic
add :: Integer -> Integer -> Integer
add x y = x + y

multiply :: Integer -> Integer -> Integer
multiply x y = x * y

-- Demonstrating immutability
main :: IO ()
main = do
    putStrLn "=== Basic Functions and Immutability ==="
    putStrLn ""

    -- Define an immutable value
    let originalBalance = 100
    putStrLn $ "Original balance: " ++ show originalBalance

    -- Create a new value by applying a function
    let newBalance = deposit originalBalance 50
    putStrLn $ "After depositing 50: " ++ show newBalance

    -- Notice: originalBalance hasn't changed!
    putStrLn $ "Original balance (unchanged): " ++ show originalBalance

    putStrLn ""
    putStrLn "Key Insight:"
    putStrLn "- originalBalance is still 100"
    putStrLn "- newBalance is 150"
    putStrLn "- We created a NEW value, not modified the old one"
    putStrLn "- This is IMMUTABILITY - the foundation of functional programming safety"

    putStrLn ""
    putStrLn "=== Other Examples ==="
    putStrLn $ "5 + 3 = " ++ show (add 5 3)
    putStrLn $ "5 * 3 = " ++ show (multiply 5 3)

{-
  To run this example:

  In GHCi (Haskell REPL):
  1. Load the file: :load basic.hs
  2. Run main: main
  3. Or test functions directly:
     - deposit 100 50
     - add 5 3
     - multiply 4 7

  To compile and run:
  1. ghc basic.hs
  2. ./basic
-}
