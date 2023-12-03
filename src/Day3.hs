module Day3
  ( part1,
    part2,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, find, nub)
import Import

example :: String
example = "src/input/day3example1.txt"

exampleSolution :: String
exampleSolution = "4361"

input :: String
input = "src/input/day3.txt"

file :: String
file = example

-- add up all the part numbers
-- part number is "adjacent" to symbol
--
-- 01234
-- 333..
-- ...*.
-- adjacent is:
-- - Number is immediately followed by a Symbol (123*)
-- - Symbol on line after Number and its index is between:
--    - (index of first digit of Number) - 1
--    - (index of last digit of Number) + 1
-- - Symbol is on line before Number and its index is between:
--    - (index of first digit of Number) - 1
--    - (index of last digit of Number) + 1

-- Reader ParsedLine  >>= (numbers, symbols) -> Reader (...)
-- runReader takes a function f, which has access to an environment e, and returns a value a
--
-- reader is a function that takes a shared state and returns some value
-- ex :: Reader SharedState Result
-- ex =
--   ask >>= \sharedState ->
--     return $ funThatDoesSomethingWithState sharedState
--
-- runReader ex mySharedState
--
-- It's kind of like >>= means "bind a callback"
--
-- refactor:
-- ex :: Reader SharedState Result
-- ex = do
--   sharedState <- ask
--   return $ funThatDoesSomethingWithState sharedState
--

type Location = (Integer, Integer)

data ParsedNumber = PartNumber Integer Location | OtherNumber Integer Location
  deriving (Show)

data Symbol = Symbol String Location
  deriving (Show)

type ParsedLine = ([ParsedNumber], [Symbol])

-- What's the shared state I need from digit to digit?
data Env = EnvData
  { currentLine :: String,
    currentlyParsingNumber :: String,
    parsedNumbers :: [ParsedNumber],
    parsedSymbols :: [Symbol]
  }

-- digit? keep in parsingNumber and append next step
-- not digit? have parsingNumber kept? return parsingNumber as Number
-- continue

parseLine :: String -> Reader Env ParsedLine
parseLine line = do
  env <- ask
  -- step <- runReader parseStep line
  return ([], [])

parseStep :: Reader Env ParsedLine
parseStep = undefined

---------- End Pt. 1 Initial Attempt -------------

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
      -- parsedLines = map (runReader parseLine) ls
      answer = "0"
  return $ show (answer == exampleSolution, answer)

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  return $ show ls
