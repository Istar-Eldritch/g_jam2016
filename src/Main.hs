module Main where
  import Prelude
  import Digits(digitsForWord)
  import System.Environment
  import System.Exit
  import Text.Read

  testCasesHelper :: Int -> Int -> [String] -> IO()
  testCasesHelper _ 0 _ = exitSuccess
  testCasesHelper _  _ [] = exitFailure
  testCasesHelper n1 n2 (h:t) = do
    putStrLn("Case #" ++ show n1 ++ ": " ++ digitsForWord h)
    testCasesHelper (n1 + 1) (n2 - 1) t

  testCases :: Int -> [String] -> IO()
  testCases = testCasesHelper 1


  main :: IO()
  main = do
    args <- getArgs
    content <- readFile $ head args
    let (h:t) = lines content
    testCases (read h) t
