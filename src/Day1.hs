module Day1 (part1) where
import Data.Char (isDigit)

example = "src/input/day1example.txt"

file = example

readLine :: String -> String
readLine = go []
  where
    go out line = case line of
      (c : cs) | isDigit c -> go (updateNumber out c) cs
               | otherwise -> go out cs
      [] -> if length out < 2 then [head out, head out] else out
    updateNumber nums n
      | null nums = [n]
      | otherwise = head nums : [n]

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
  let result = map readLine ls
  print result
  -- _ <- map putStrLn result
  -- putStrLn $ head result
  return ""
