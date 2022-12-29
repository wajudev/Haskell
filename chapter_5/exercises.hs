import Data.Char
-- 1. Using list comprehension, give an expression tht calculates the sum of the first 100 squares
sumofsquares :: Int -> Int
sumofsquares n = sum[x^2 | x <- [1..n]]

-- 2. Suppose that a coordinate grid of size m x n is given by the list of all pairs (x,y) of integers such that
--    0 <= x <= m and 0 <= y <= n. Using a list comprehension, define a function grid :: Int -> Int -> [(Int, Int)] that
--    returns a coordinate grid of a given size.

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3. Using a list comprehension and the function grid above, define a function square :: Int -> [(Int, Int)] that returns
--    a coordinate square of size n, excluding the diagonal from (0,0) to (n,n).

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

--- 4. Show how the library function replicate :: Int -> a -> [a] that produces a list of identical elements can be
--     definned using a list comprehension
replicate_new :: Int -> a -> [a]
replicate_new n x = [x | _ <- [1..n]]

-- 5. A triple (x,y,z) of positive integers is Pythagorean if it satisfies the equation x^2 + y^2 = z^2. Using a list
--    comprehension with three generators, define a function pyths :: Int -> [(Int, Int, Int)] that returns the list of
--    all such triples whose components are at most a given limit
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]

-- 6. A positive integer is perfect if it equals the sum of all its factors excluding the number itself. Using a list
--    comprehension and the function factors, define a function perfects :: Int -> [Int] that returns the list of all
--    perfect numbers up to a given limit

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(init(factors x)) == x]

-- 7. show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators can be re-expressed using
--    two comprehension with single generators.
-- Hint: nest one comprehension within the other and make use of the library function concat :: [[a]] -> [a] .

combine :: [(Int, Int)]
combine = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- 8. Redefine the function positions using the function find.
-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x', i) <- zip xs [0..], x == x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
                 where n = length xs - 1

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- 9. The scalar product of two lists of Integers xs and ys of length n is given by the sum of the products of
--    corresponding integers, similar to chisqr show how a list comprehension can be used to define a function
--    scalarproduct :: [Int] -> [Int] -> Int that returns scalar product of two lists
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 10. Modify the Caeser cipher program to also handle upper-case letters


letlower2int :: Char -> Int
letlower2int c = (ord c - ord 'a')

int2letlower :: Int -> Char
int2letlower n = chr (ord 'a' + n)

letupper2int   :: Char -> Int
letupper2int c = ord c - ord 'A'

int2letupper   :: Int -> Char
int2letupper n =  chr(ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2letlower ((letlower2int c + n) `mod` 26)
          | isUpper c = int2letupper ((letupper2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

------------decipher-----------------------------
-- returns the number of lower case letters
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- return the occurence of a particular character
count :: Char -> [Char] -> Int
count c xs = sum [1 | x <- xs, x == c]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

letters :: [Char] -> Int
letters xs = sum [1 | x <- xs, isLetter x]

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = letters xs

