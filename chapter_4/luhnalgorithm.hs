-- The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows
-- 1. consider each digit as a seperate number
-- 2. moving left, double every other number from the second last
-- 3. subtract 9 from each number that is now greater than 9
-- 4. addall the resulting numbers together
-- 5. if the total is divisible by 10, the card number is valid

luhnDouble :: Int -> Int
luhnDouble x | (*2) x > 9 = (*2) x - 9
             | otherwise = (*2) x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4 | sum [luhnDouble x1, x2, luhnDouble x3, x4] `mod` 10 == 0 = True
                 | otherwise = False
