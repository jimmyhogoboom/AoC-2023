module Day3
  ( part1,
    part2,
  )
where

import Data.Char (isSpace, isDigit)
import Data.List (dropWhileEnd, find, nub)
import Import

example :: String
example = "src/input/day3example1.txt"

exampleSolution :: String
exampleSolution = "4361"

puzzleInput :: String
puzzleInput = "src/input/day3.txt"

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

newtype LineNumber = LineNumber Integer
  deriving (Show)

newtype CharacterNumber = CharacterNumber Integer
  deriving (Show)

type Location = (LineNumber, CharacterNumber)

data ParsedNumber = PartNumber Integer Location | OtherNumber Integer Location
  deriving (Show)

data Symbol = Symbol String Location
  deriving (Show)

type ParsedLine = ([ParsedNumber], [Symbol])

-- What's the shared state I need from digit to digit?
data Env = EnvData
  { currentLine :: String,
    lineNumber :: Integer,
    characterNumber :: Integer,
    currentlyParsingValue :: String,
    parsedNumbers :: [ParsedNumber],
    parsedSymbols :: [Symbol]
  }
  deriving (Show)

-- digit? keep in parsingNumber and append next step
-- not digit? have parsingNumber kept? return parsingNumber as Number
-- continue

type Parser = Env -> String -> Env

parseChar :: (Char -> Bool) -> Parser
parseChar match env@(EnvData {currentlyParsingValue = n}) input =
  case input of
    [] -> env
    (c : cs)
      | match c ->
          env
            { currentlyParsingValue = n ++ [c],
              currentLine = cs,
              characterNumber = characterNumber env + 1
            }
      -- Store the number we were parsing when it's done
      | not (null (currentlyParsingValue env)) ->
          let lineNumber' = LineNumber $ lineNumber env
              characterNumber' = CharacterNumber $ characterNumber env
              number = read [c] :: Integer
              parsedNumber = PartNumber number (lineNumber', characterNumber')
           in env
                { parsedNumbers = parsedNumber : parsedNumbers env,
                  currentLine = cs,
                  characterNumber = characterNumber env + 1
                }
      -- TODO: parse symbols
      | otherwise ->
          env
            { currentLine = cs,
              characterNumber = characterNumber env + 1
            }

runParser :: Parser -> String -> Env
runParser p i =
  p (EnvData i 0 0 "" [] []) i

---------- End Pt. 1 Initial Attempt -------------

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
      parseC = parseChar isDigit
      parseLine = runParser parseC
      parsedLine = parseLine $ head ls
      -- parsedLines = map (map (runParser parseChar)) ls
      answer = "0"
  return $ show parsedLine

-- return $ show (answer == exampleSolution, answer)

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  return $ show ls
