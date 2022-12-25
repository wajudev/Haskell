sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs

qsortDesc [] = []
qsortDesc(x:xs) = qsortDesc greaterThan ++ [x] ++ qsortDesc lesserThan
                    where
                        greaterThan = [a | a <- xs, a >= x]
                        lesserThan = [b | b <- xs, b < x]