-- List comprehensions
odds :: Int -> [Int]
odds n = [x*2 + 1 | x <- [0..n-1]]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n] -- Uses lazy evaluation

-- Produces list of prime numbers up to a given limit
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- Produces a list of value with matching keys
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
