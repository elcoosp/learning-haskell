
-- Substract from s of half it's argument
subHalf :: Fractional a => a -> a -> a
subHalf s x = (x/2) - s

subHalf12 = subHalf 12

-- Calculate the volume of a box
boxVol :: Num a => a -> a -> a -> a
boxVol l w h = l * w * h 

-- Number of blocks of b volume in a p volume
pyrBlocksNum :: Fractional a => a -> a -> a
pyrBlocksNum b p = p / b
kheopsBlocksNum = pyrBlocksNum 1.10 2592341

-- Circle area
area :: Floating a => a -> a
area r = pi * r ^ 2

-- Calculate the volume of a cylinder
cylinderVol :: Floating a => a -> a -> a
cylinderVol r h =  area r * h 