module Day1 (part1) where

import Data.Char (isDigit)

example = "src/input/day1example.txt"

input = "src/input/day1.txt"

file = input

readLine :: String -> String
readLine = go []
  where
    go out line = case line of
      (c : cs)
        | isDigit c -> go (updateNumber out c) cs
        | otherwise -> go out cs
      [] -> out
    updateNumber nums newNum = case nums of
      (n : _) -> [n, newNum]
      [] -> [newNum, newNum]

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
  let result = map readLine (filter (not . null) ls)
  return $ show $ sum $ map read result
