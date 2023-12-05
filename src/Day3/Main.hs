module Day3.Main
  ( part1,
    part2,
  )
where

import Day3.Part1

example :: String
example = "src/input/day3example1.txt"

exampleSolution :: String
exampleSolution = "4361"

puzzleInput :: String
puzzleInput = "src/input/day3.txt"

testInput = "src/input/day3test.txt"

file :: String
file = puzzleInput

-- adjacent is:
-- - Number is immediately followed by a Symbol (123*)
-- - Symbol on line after Number and its index is between:
--    - (index of first digit of Number) - 1
--    - (index of last digit of Number) + 1
-- - Symbol is on line before Number and its index is between:
--    - (index of first digit of Number) - 1
--    - (index of last digit of Number) + 1

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
      parts = parsePartNumbers ls
      answer = sumOfParts parts
      -- partNumbers = map partNumber parts
  return $ show answer

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  return $ show ls
