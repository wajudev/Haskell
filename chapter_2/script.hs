double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Integer -> Integer
factorial n = product [1..n]

average :: [Int] -> Int
average ns = sum ns `div` length ns
--average ns = div (sum ns) (length ns)