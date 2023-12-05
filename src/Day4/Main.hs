module Day4.Main
  ( part1,
    part2,
  )
where

import Day4.Part1
import Day4.Part2

example :: String
example = "src/input/day4example1.txt"

puzzleInput :: String
puzzleInput = "src/input/day4.txt"

file :: String
file = example

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
      answer = "0"
  return $ show answer

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
      answer = "0"
  return $ show answer

