-- add :: (Int, Int) -> Int
-- add (x,y) = x + y
----------- curry ----------------------
add :: Int -> Int -> Int
add x y = x+y

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z
-- ((mult x)y)z
-- Currying is important except tupling is explicitly stated.

