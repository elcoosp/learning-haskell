-- Conses a num (at the beginning) of a list
cons' :: Num a => a -> [a] -> [a]
cons' x xs = x : xs

-- Conses 8
cons8 :: Num a => [a] -> [a]
cons8 = cons' 8

-- Append to end of a list
append :: a -> [a] -> [a]
append x xs = xs ++ [x]

-- Snd value from first tuple
sndInFirst :: ((a, b1), b2) -> b1
sndInFirst t = snd (fst t) 

-- Returns the head and the tail of a list as the first and second elements of a tuple
headTailTuple  :: [a] -> (a, [a])
headTailTuple [] = error "The array is empty"
headTailTuple xs = (head xs, tail xs) 

-- Gives the fifth element of a list
fifth :: [a] -> a
fifth xs = xs !! 5