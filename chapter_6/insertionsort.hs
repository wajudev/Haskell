import Test.QuickCheck

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

prop_idempotent xs = isort (isort xs) == isort xs
prop_minimum' xs = not (null xs) ==> head (isort xs) == minimum xs