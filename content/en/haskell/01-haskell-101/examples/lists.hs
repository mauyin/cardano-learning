{-
  List Operations and Higher-Order Functions in Haskell

  This example demonstrates:
  - List syntax and operations
  - Higher-order functions (map, filter, foldr, etc.)
  - Lambda functions
  - Function composition
  - Why lists are fundamental in functional programming
-}

-- Basic list examples
numbers :: [Integer]
numbers = [1, 2, 3, 4, 5]

names :: [String]
names = ["Alice", "Bob", "Charlie"]

-- Using map: apply a function to every element
-- map :: (a -> b) -> [a] -> [b]
squaredNumbers :: [Integer]
squaredNumbers = map (\x -> x * x) numbers
-- Using lambda: \x -> x * x means "a function that takes x and returns x squared"

doubledNumbers :: [Integer]
doubledNumbers = map (*2) numbers
-- (*2) is partial application: creating a function that multiplies by 2

-- Using filter: keep only elements that satisfy a condition
-- filter :: (a -> Bool) -> [a] -> [a]
evenNumbers :: [Integer]
evenNumbers = filter even numbers

oddNumbers :: [Integer]
oddNumbers = filter odd numbers

greaterThanThree :: [Integer]
greaterThanThree = filter (>3) numbers

-- Using foldr: reduce a list to a single value
-- foldr :: (a -> b -> b) -> b -> [a] -> b
sumAll :: Integer
sumAll = foldr (+) 0 numbers
-- This says: "fold right using (+), starting with 0"
-- foldr (+) 0 [1,2,3,4,5]
-- = 1 + (2 + (3 + (4 + (5 + 0))))
-- = 15

productAll :: Integer
productAll = foldr (*) 1 numbers

-- Function composition: combining multiple operations
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
processNumbers :: [Integer] -> Integer
processNumbers = foldr (+) 0 . map (*2) . filter even
-- Read from right to left:
-- 1. filter even: keep even numbers
-- 2. map (*2): double each number
-- 3. foldr (+) 0: sum them all

-- List comprehensions (similar to Python)
squaresComprehension :: [Integer]
squaresComprehension = [x * x | x <- [1..10]]

evenSquares :: [Integer]
evenSquares = [x * x | x <- [1..10], even x]

-- Practical example: working with pairs
pairs :: [(String, Integer)]
pairs = [("Alice", 25), ("Bob", 30), ("Charlie", 35)]

-- Extract names
extractNames :: [(String, Integer)] -> [String]
extractNames = map fst  -- fst gets the first element of a pair

-- Filter by age
olderThan30 :: [(String, Integer)] -> [(String, Integer)]
olderThan30 = filter (\(name, age) -> age > 30)

main :: IO ()
main = do
    putStrLn "=== Lists and Higher-Order Functions ==="
    putStrLn ""

    putStrLn "Original list:"
    putStrLn $ "numbers = " ++ show numbers

    putStrLn ""
    putStrLn "Using map (transform each element):"
    putStrLn $ "squared = " ++ show squaredNumbers
    putStrLn $ "doubled = " ++ show doubledNumbers

    putStrLn ""
    putStrLn "Using filter (keep elements matching condition):"
    putStrLn $ "even numbers = " ++ show evenNumbers
    putStrLn $ "odd numbers = " ++ show oddNumbers
    putStrLn $ "greater than 3 = " ++ show greaterThanThree

    putStrLn ""
    putStrLn "Using foldr (reduce to single value):"
    putStrLn $ "sum = " ++ show sumAll
    putStrLn $ "product = " ++ show productAll

    putStrLn ""
    putStrLn "Function composition:"
    putStrLn $ "processNumbers [1..10] = " ++ show (processNumbers [1..10])
    putStrLn "  (filters even, doubles, then sums)"

    putStrLn ""
    putStrLn "List comprehensions:"
    putStrLn $ "squares 1-10 = " ++ show squaresComprehension
    putStrLn $ "even squares = " ++ show evenSquares

    putStrLn ""
    putStrLn "Working with pairs:"
    putStrLn $ "pairs = " ++ show pairs
    putStrLn $ "names only = " ++ show (extractNames pairs)
    putStrLn $ "older than 30 = " ++ show (olderThan30 pairs)

    putStrLn ""
    putStrLn "Key Insights:"
    putStrLn "- map: transform each element"
    putStrLn "- filter: keep elements matching a condition"
    putStrLn "- foldr: reduce list to single value"
    putStrLn "- Lambda: \\x -> x * 2 is an anonymous function"
    putStrLn "- Composition: combine functions with (.)"
    putStrLn "- All these operations create NEW lists, never modify originals!"

{-
  To run this example:

  In GHCi:
  1. :load lists.hs
  2. main
  3. Or test directly:
     - map (*3) [1,2,3,4,5]
     - filter (>10) [5,10,15,20]
     - foldr (+) 0 [1,2,3,4,5]

  To compile and run:
  1. ghc lists.hs
  2. ./lists

  Challenge: Can you use these functions to:
  1. Find the sum of all odd numbers from 1 to 100?
     foldr (+) 0 (filter odd [1..100])

  2. Get the length of all names longer than 4 characters?
     length (filter (\name -> length name > 4) names)
-}
