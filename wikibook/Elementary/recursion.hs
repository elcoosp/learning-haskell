-- Product of every other number from 1 (or 2) up to n
doublefactorial :: Int -> Int
doublefactorial n
    | n <= 0 = 1
    | n >= 1 = doublefactorial (n - 2) * n

-- Put a int x to the y power
power :: (Num a, Eq a) => a -> a -> a
power 0 _ = 0
power _ 0 = 1
power x y = x * power x (y - 1)

-- Add one
plusOne :: Num a => a -> a
plusOne x = x + 1

-- Add two ints together
addition :: Int -> Int -> Int
addition x y
    | y == 0 = x
    | y < 0 = addition (x - 1) (plusOne y)
    | otherwise =  addition (plusOne x) (y - 1)

-- Log2
log2 :: Fractional a => a -> a
log2 = logBase 2