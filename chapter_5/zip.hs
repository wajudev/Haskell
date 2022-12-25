-- zip function produces a new list by pairing successive elements from existing lists until either or both lists are exhausted

-- Produces a list of pairs of adjacent element from a single list
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- Decides if a list of elemnets of any ordered type is sorted by checking that all pairs of adjacent elements from the list are in correct order
sorted :: Ord a => [a] -> Bool
sorted xs = and[x <= y | (x,y) <- pairs xs] -- Uses lazy evaluation

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x'] -- Uses Lazy evaluation eventhough [0..] is notionally infinite, only elements in the provided list are considered
jl