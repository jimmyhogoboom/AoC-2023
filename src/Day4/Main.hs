module Day4.Main
  ( part1,
    part2,
  )
where

import Day4.Part1
import Day4.Part2
import Data.Maybe (fromMaybe, fromJust)

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
      cards = fromMaybe [] $ parseLines ls
      cards' = map winners cards
      scores = map score cards'
      answer = sum scores
  return $ show answer

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
      cards = fromMaybe [] $ parseLines ls
      winMap = cardWinMap cards
      cardNumbers = map cardNumber cards
      copies' = getCopies' winMap cardNumbers
      answer = length $ fst copies'
  return $ show answer

