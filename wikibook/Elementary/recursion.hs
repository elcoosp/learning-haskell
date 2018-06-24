-- Product of every other number from 1 (or 2) up to n
doubleFactorial :: Int -> Int
doubleFactorial n
  | n <= 0 = 1
  | n >= 1 = doubleFactorial (n - 2) * n

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
  | otherwise = addition (plusOne x) (y - 1)

-- Log base 2
log2 :: Double -> Double
log2 = logBase 2

-- ########################
-- #### List recursion ####
-- ########################
--
-- Takes a count and an element and returns the list which is that element repeated that many times.
replicate' :: Int -> a -> [a]
replicate' i x =
  case (i, x) of
    (0, _) -> []
    _ -> x : replicate' (i - 1) x

-- Element at the given index
atIdx :: [a] -> Int -> a
atIdx (x:xs) i
  | i > length xs || i < 0 = error "Index out of range"
  | i == 0 = x
  | otherwise = atIdx xs (i - 1)

-- Return a tuple of the heads from the two lists
heads :: [a] -> [b] -> (a, b)
heads x y = (head x, head y)

-- Takes two lists and 'zips' them together, so that the first pair in the resulting list is the first two elements of the two lists, and so on
zip' :: [a] -> [b] -> [(a, b)]
zip' xs xxs
  | length xs == 1 || length xxs == 1 = [heads xs xxs]
  | otherwise = heads xs xxs : zip' (tail xs) (tail xxs)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = length xs + 1
