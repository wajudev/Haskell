-- 1. Using library functions define a function halve :: [a] -> ([a], [a])
--    that splits an even-lengthed list into two halves

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

-- 2. Define a function third :: [a] -> a that returns the third element in a list
-- that contains at least this many elements using:
-- a. head and tail
thirdA :: [a] -> a
thirdA xs = head (tail(tail xs))

-- b. list indexing !!
thirdB :: [a] -> a
thirdB xs = xs !! 2

-- c. pattern matching
thirdC :: [a] -> a
thirdC (_:_:x:_) = x

-- 3. Consider a function safetail :: [a] -> [a] that behaves in the same way as tail except that it maps the empty list
-- to itself rather than producing an error. Using tail and the function null :: [a] -> Bool that decides if a list is
-- empty or not, define safetail using

--a. A conditional expression
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

--b. Guarded equations
safetailB :: [a] -> [a]
safetailB xs | null xs = xs
             | otherwise = tail xs

--c. Pattern Matching
safetailC :: [a] -> [a]
safetailC [] = []
safetailC (_:xs) = xs
