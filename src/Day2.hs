module Day2 (part1) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, find)

example :: String
example = "src/input/day2example1.txt"

example2 :: String
example2 = "src/input/day2example2.txt"

input :: String
input = "src/input/day2.txt"

file :: String
file = example

-- Cubes: red, green, blue
--
-- The Elf would first like to know which games would have been possible
-- if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

-- for each value, check against matching value in test Game
--
-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green

wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitOn s = wordsWhen (== s)

trim = dropWhileEnd isSpace . dropWhile isSpace

type PickPart = (Integer, String)

type Pick = [PickPart]

valueFromS :: String -> PickPart
valueFromS s = (read $ head vals, last vals)
  where
    vals = splitOn ' ' s

picksFromValues :: String -> [Pick]
picksFromValues values =
  let picks = splitOn ';' values
      picks' = map (splitOn ',' . trim) picks
      picks'' = map (map valueFromS) picks'
   in picks''

gameAndValues :: String -> [String]
gameAndValues line =
  let [gameId, valuesString] = splitOn ':' line
      [_, gameId'] = splitOn ' ' gameId
   in [gameId', valuesString]

gameAndPicks :: String -> (Integer, [Pick])
gameAndPicks line =
  let [gameId, valuesString] = gameAndValues line
   in (read gameId, picksFromValues valuesString)

testBag :: [(String, Integer)]
testBag = [("red", 12), ("green", 13), ("blue", 14)]

isPartValid :: PickPart -> Bool
isPartValid (c, t) = case found of
  Just (_, limit) -> c <= limit
  Nothing -> False
  where
    found = find ((== t) . fst) testBag

isPickValid :: Pick -> Bool
isPickValid = all isPartValid

isGameValid :: (Integer, [Pick]) -> Bool
isGameValid (_, picks) = all isPickValid picks

-- TODO: This pattern keeps coming up. There must be a better way
sumValidGameIds :: [(Integer, [Pick])] -> Integer
sumValidGameIds = go 0
  where
    go total gs = case gs of
      (g : rest)
        | isGameValid g -> go (total + fst g) rest
        | otherwise -> go total rest
      [] -> total

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
  let games = map gameAndPicks ls
  let (gameId, picks) = head games
  return $ show $ sumValidGameIds games
