{-
  Recursion and Pattern Matching in Haskell

  This example demonstrates:
  - Recursion (function calling itself)
  - Pattern matching (defining different behaviors for different inputs)
  - Base case and recursive case
  - How FP replaces loops with recursion
-}

-- Type signature: takes an Integer, returns an Integer
factorial :: Integer -> Integer

-- Pattern matching: we define two different behaviors
-- First pattern: when input is 0, return 1 (BASE CASE)
factorial 0 = 1

-- Second pattern: for any other number n (RECURSIVE CASE)
-- Calculate n * factorial(n-1)
factorial n = n * factorial (n - 1)

-- Let's trace through factorial 3:
-- factorial 3
-- = 3 * factorial 2
-- = 3 * (2 * factorial 1)
-- = 3 * (2 * (1 * factorial 0))
-- = 3 * (2 * (1 * 1))          -- Hit base case!
-- = 3 * (2 * 1)
-- = 3 * 2
-- = 6

-- Another example using guards (conditional pattern matching)
factorialWithGuards :: Integer -> Integer
factorialWithGuards n
    | n <= 0    = 1  -- Guard: if n <= 0, return 1
    | otherwise = n * factorialWithGuards (n - 1)

-- Fibonacci: another classic recursion example
fibonacci :: Integer -> Integer
fibonacci 0 = 0  -- Base case 1
fibonacci 1 = 1  -- Base case 2
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  -- Recursive case

main :: IO ()
main = do
    putStrLn "=== Recursion and Pattern Matching ==="
    putStrLn ""

    putStrLn "Factorial Examples:"
    putStrLn $ "factorial 0 = " ++ show (factorial 0)
    putStrLn $ "factorial 1 = " ++ show (factorial 1)
    putStrLn $ "factorial 3 = " ++ show (factorial 3)
    putStrLn $ "factorial 5 = " ++ show (factorial 5)
    putStrLn $ "factorial 10 = " ++ show (factorial 10)

    putStrLn ""
    putStrLn "Key Insights:"
    putStrLn "- Pattern matching: factorial 0 = 1 matches when input is exactly 0"
    putStrLn "- Recursive case: factorial n = n * factorial (n-1) matches all other numbers"
    putStrLn "- Haskell checks patterns from top to bottom"
    putStrLn "- The base case prevents infinite recursion"

    putStrLn ""
    putStrLn "Fibonacci Examples:"
    putStrLn $ "fibonacci 0 = " ++ show (fibonacci 0)
    putStrLn $ "fibonacci 1 = " ++ show (fibonacci 1)
    putStrLn $ "fibonacci 5 = " ++ show (fibonacci 5)
    putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10)

    putStrLn ""
    putStrLn "Note: Fibonacci with two base cases!"
    putStrLn "fibonacci 0 = 0"
    putStrLn "fibonacci 1 = 1"
    putStrLn "fibonacci n = fibonacci (n-1) + fibonacci (n-2)"

{-
  To run this example:

  In GHCi:
  1. :load factorial.hs
  2. main
  3. Or test directly:
     - factorial 5
     - fibonacci 7
     - factorialWithGuards 4

  To compile and run:
  1. ghc factorial.hs
  2. ./factorial

  Challenge: Can you write a recursive function to sum a list?
  sumList :: [Integer] -> Integer
  sumList [] = 0
  sumList (x:xs) = x + sumList xs
-}
