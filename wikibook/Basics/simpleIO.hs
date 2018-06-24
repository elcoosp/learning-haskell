import Control.Monad

-- Put a string on the output and get a line from the input
putGetLn :: String -> IO String
putGetLn s = do
    putStrLn s
    getLine

-- Put many string on the output and get a line after each.
putGetManyLn :: [String] -> IO [String]
putGetManyLn  = mapM putGetLn

-- Put on output the result of f applied to the Io String from putGetLn
putGetPutLn :: String -> (String -> String) -> IO ()
putGetPutLn s f = do
    x <- putGetLn s
    putStrLn (f x)

-- Ask the user for the base and height of a right angled triangle, calculates its area, and prints it to the screen
areaRightAngleTriangle :: IO ()
areaRightAngleTriangle = do
    [b, h] <- putGetManyLn ["The base?", "The height?"]
    print (1/2 * read b * read h)

-- Give a string depending on name input
strFromName :: String -> String
strFromName n 
    | n `elem` ["Simon","John","Phil"] = "Haskell is a great programming language"
    | n == "Koen" = "I think debugging Haskell is fun"
    | otherwise = "I don't know who you are"

-- Ask a name and put the result of strFromName to the output
askNameAndJudge :: IO ()
askNameAndJudge =
    putGetPutLn "What's your name ?" strFromName