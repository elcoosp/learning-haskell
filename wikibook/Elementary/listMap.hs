import Data.List

-- Returns the first n items in a list
takeInt :: Int -> [a] -> [a]
takeInt 0 _ = []
takeInt _ [] = []
takeInt i (x:xs) = x : takeInt (i - 1) xs

-- Drops the first n items in a list and returns the rest
dropInt :: Int -> [a] -> [a]
dropInt 0 xs = xs
dropInt _ [] = []
dropInt i (x:xs) = dropInt (i - 1) xs

-- Sum of the items in a list
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Adds the items in a list and returns a list of the running totals
scanSum' :: [Int] -> [Int]
scanSum' [] = []
scanSum' [x] = [x]
scanSum' (x:y:xs) = x : scanSum' ((x + y) : xs)

diff' :: [Int] -> [Int]
diff' ints =
  case ints of
    [] -> []
    [x] -> []
    (x:y:ys) -> (y - x) : diff' (y : ys)

-- Element-wise negation of xs
negMap :: [Int] -> [Int]
negMap = map (* (-1))

-- ALl divisors of x
divisors :: Int -> [Int]
divisors x = [f | f <- [1 .. x], x `mod` f == 0]

-- For each element of xs, contains the divisors of xs
divMap :: [Int] -> [[Int]]
divMap = map divisors

-- Element-wise negation of divisors from each int
divNegMap :: [Int] -> [[Int]]
divNegMap ints = map negMap (divMap ints)

-- Run Length Encoding (RLE) encoder and decoder
encodeRLE :: String -> [(Int, Char)]
encodeRLE s = map (\c -> (length s, head c)) (group s)

decodeRLE :: [(Int, Char)] -> String
decodeRLE [] = []
decodeRLE s = concatMap (uncurry replicate) s

--  Give the last element of the list
last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (x:xs) = last' xs

-- Give the list with the last element dropped
init' :: [a] -> [a]
init' [] = error "Empty list"
init' (x:y:xs) = x : init' (y : xs)
init' (x:_) = []

main = print $ decodeRLE [(4, 'a'), (2, 'b'), (3, 'a')]
