n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- last xs returns last in list, weird way of doing it
lastInList :: Num a => [a] -> a
lastInList xs = head (reverse xs)

lastInList2 :: Num a => [a] -> a
lastInList2 xs = xs !! (length xs - 1)

initListItem xs = init xs

