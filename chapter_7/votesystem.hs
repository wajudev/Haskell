import Test.QuickCheck
import Data.List
-- First past the post
votes :: [String]
votes = ["Orange", "Red", "Blue", "Red", "Green", "Blue", "Orange", "Red", "Red", "Blue"]

-- count number of occurence
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- removes duplicate values from list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

