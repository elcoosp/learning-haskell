-- Sign swap
myNegate :: Num a => a -> a
myNegate a = -a

-- ||
myOr :: Bool -> Bool -> Bool
myOr False False = False
myOr _ _  = True

-- Check if num is pair
isPair :: Int -> Bool
isPair x = x `mod` 2 == 0

-- Length of month of 30/31 days
normalMonthLength :: Int -> Int
normalMonthLength m = if isPair m then 30 else 31

-- Days in a month (with leap year or not)
monthLength :: Bool -> Int -> Int
monthLength True m = case m of 
    2 -> 29
    x -> normalMonthLength x
monthLength False m = case m of 
    2 -> 28
    x -> normalMonthLength x

-- Types to find
isNotAndIsTrue :: Bool -> Bool -> Bool
isNotAndIsTrue x y = not x && y

doMath :: Num a => a -> a
doMath x = (2 * x - 1) ^ 2