module Day2 (part1, part2) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, find, nub)
import Import

example :: String
example = "src/input/day2example1.txt"

input :: String
input = "src/input/day2.txt"

file :: String
file = input

-- The Elf would first like to know which games would have been possible
-- if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

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
gameAndValues line = case parts of
  [gameId, valuesString] -> [getId gameId, valuesString]
  _ -> []
  where
    parts = splitOn ':' line
    getId = last . splitOn ' '

gameAndPicks :: String -> (Integer, [Pick])
gameAndPicks line =
  let parts = gameAndValues line
   in case parts of
        [gameId, valuesString] -> (read gameId, picksFromValues valuesString)
        _ -> (0, [])

-------------- Part 1 --------------

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
  return $ show $ sumValidGameIds games

----------- Part 2 --------------

-- Game: (Integer, [Pick])

partType :: PickPart -> String
partType = snd

partCount :: PickPart -> Integer
partCount = fst

partByType :: String -> Pick -> Maybe PickPart
partByType t = find ((== t) . partType)

findPartCount :: [PickPart] -> String -> Integer
findPartCount parts part =
  partCount $ fromMaybe (0, part) (partByType part parts)

maxPart :: [Pick] -> String -> PickPart
maxPart picks t = go (0, "") (join picks)
  where
    go maxP parts = case parts of
      (part : ps) ->
        if partType part == t && partCount maxP < partCount part
          then go part ps
          else go maxP ps
      [] -> maxP

maxesOfGame :: (Integer, [Pick]) -> (Integer, [PickPart])
maxesOfGame (gameId, picks) = (gameId, maxEachType picks)
  where allTypes p = nub $ map partType (join p)
        maxEachType p = map (maxPart p) (allTypes p)

powerOfGame :: (Integer, [PickPart]) -> Integer
powerOfGame game = product (gameCounts game)
  where gameCounts (_, parts) = map partCount parts

gamePower :: String -> Integer
gamePower gameString = let
  game = gameAndPicks gameString
  maxes = maxesOfGame game
  power = powerOfGame maxes
  in power

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  let sumOfPowers = sum $ map gamePower ls
  return $ show sumOfPowers
