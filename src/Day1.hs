{-# LANGUAGE NoImplicitPrelude #-}
module Day1 (part1, part2) where

import Import
import Data.Char (isDigit)
import System.IO (readFile)

example :: String
example = "src/input/day1example.txt"

example2 :: String
example2 = "src/input/day1example2.txt"

input :: String
input = "src/input/day1.txt"

file :: String
file = input

readLine1 :: String -> String
readLine1 = go []
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
  let result = map readLine1 (filter (not . null) ls)
  let nums = mapM readMaybe result :: Maybe [Integer]
  case nums of
    Nothing -> return "Didn't find any numbers"
    Just ns -> return $ show $ sum ns

-- Begin part 2 --

    -- TODO: change to [(String, String)]
numberWords :: [(String, Char)]
numberWords =
  [ ("one", '1'),
    ("two", '2'),
    ("three", '3'),
    ("four", '4'),
    ("five", '5'),
    ("six", '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine", '9'),
    ("zero", '0')
    -- TODO: add support for numbers up to 99
  ]

mapKeys inputMap = map fst inputMap

isNumberWord word = word `elem` (mapKeys numberWords)

numberFromWord word = lookup word numberWords

-- Get a line, return
--  - a parsed number with the rest of the line ((Just '0', []))
--  - Nothing with the rest of the line (Nothing, "therestoftheline")
parseNum :: String -> (Maybe Char, String)
parseNum = go ""
  where
    go test rest = case rest of
      (d : ds)
        | isNumberWord newTest -> (numberFromWord newTest, ds)
        | otherwise -> go newTest ds
        where newTest = test ++ [d]
      [] -> (Nothing, rest)

readLine :: String -> Maybe Int
readLine = readMaybe . go []
  where
    go out line = case line of
      (c : cs)
        | isDigit c -> go (updateNumber out c) cs
        | otherwise ->
            let parseResult = parseNum line
             in case parseResult of
                  (Nothing, _) -> go out cs
                  (Just parsed, remainder) -> go (updateNumber out parsed) cs
      [] -> out
    updateNumber nums newNum = case nums of
      (n : _) -> [n, newNum]
      [] -> [newNum, newNum]

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = filter (not . null) (lines contents)
  let result = mapM readLine ls
  case result of
    Nothing -> return "Didn't find any numbers"
    Just ns -> return $ show $ sum ns
