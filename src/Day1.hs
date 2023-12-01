module Day1 (part1) where

example = "src/input/day1example.txt"

file = example

part1 :: IO String
part1 = do
  contents <- readFile file
  return contents
