import System.Console.ANSI

-- Wether the action succeeded ot not, and an error message (or empty string)
type InputValidation = (Bool, String)

putColorPrefix :: (ColorIntensity, Color) -> String -> String -> IO ()
putColorPrefix (intensity, color) prefix text = do
  setSGR [SetColor Foreground intensity color]
  putStr prefix
  setSGR [Reset]
  putStrLn (" " ++ text)

printQuestion = putColorPrefix (Dull, Cyan) "?"

printError errType = putColorPrefix (Vivid, Red) ("ERROR:" ++ " " ++ errType)

promptInput :: String -> (String -> InputValidation) -> IO String
promptInput q isValid = do
  printQuestion q
  input <- getLine
  case isValid input of
    (True, _) -> return input
    (_, err) -> do
      printError "Input is invalid" err
      promptInput q isValid

isStrA :: String -> InputValidation
isStrA "a" = (True, "")
isStrA _ = (False, "Character need to be a")

i = promptInput "gfg" isStrA
