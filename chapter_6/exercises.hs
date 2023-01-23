-- 1. How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)?
--    Modify the definiton to prohibit negative arguments by adding guard to the recursive case.

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- 2. Define a recursive function sundown :: Int -> Int that returns the sum of the non-negative integers from a given
--    value down to zero.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown(n-1)

-- 3. Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the multiplication
--    operator *
-- (^) :: Int -> Int -> Int
-- m ^ 0 = 1
-- m ^ n = m * (m ^ (n-1))

--4. Define a recursive euclid :: Int -> Int -> Int that implements Euclid's algorithm for calculating the greatest common
--   divisor of two non-negative numbers:
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | y < x = euclid (x-y) y

-- 6.  Without looking at the definitions from the standard prelude, define the following library functions using recursion.

-- a. Decide if all logical values in a list are True
and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs

--b. Concatenate a list of lists
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

--c produce a list with n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x a  = [a] ++ replicate' (x - 1) a

--d select the nth element in a list
nth' :: [a] -> Int -> a
nth' (a:_) 0 = a
nth' (a:as) n = nth' as (n-1)

--e Decide if a value is an element of a list
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (a:as) | x == a = True
               | otherwise = elem' x as

--9. Using the five-step process, define the library functions that
-- a. calculate the sum of a list of numbers,
-- b. take a given number of elements from the start of a list
-- c. select the last element of a non-empty list.

-- Five Step Process
     --  1 - define the type
     --  2 - enumerate the cases
     --  3 - define the simple cases
     --  4 - define the other cases
     --  5 - generalise and simplify

-- a.
--Step 1
-- sum' :: [Int] -> Int
--Step 2
-- sum' [] =
-- sum' xs =
--Step 3
-- sum' [] = 0
-- sum' (x:xs) =
--Step 4
-- sum' [] = 0
-- sum' (x:xs) = x + sum xs
--Step 5
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs