module Day4.Main
  ( part1,
    part2,
  )
where

import Day4.Part1
import Day4.Part2
import Data.Maybe (fromMaybe)

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
      total = map winnerCount cards
      winMap = cardWinMap cards
      -- copies = getCopies winMap $ cardNumber $ head cards
      cardNumbers = map cardNumber cards
      winRec = findWinsRecord winMap $ last cardNumbers
      copies = getCopies winMap ([], [last cardNumbers])
      -- allCopies = getAllCopies cards
      answer = sum total
  return $ show winRec

