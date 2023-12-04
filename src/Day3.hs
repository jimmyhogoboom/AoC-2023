module Day3
  ( part1,
    part2,
  )
where

import Data.Char (isDigit, isSpace)
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

newtype Column = Column Integer
  deriving (Show, Eq)
instance Num Column where
  Column c1 + Column c2 = Column (c1 + c2)
  Column c1 - Column c2 = Column (c1 - c2)
  fromInteger = Column

type Location = (LineNumber, (Column, Column))

data ParsedNumber = PartNumber Integer Location | OtherNumber Integer Location
  deriving (Show)

data Symbol = Symbol String Location
  deriving (Show)

type ParsedLine = ([ParsedNumber], [Symbol])

-- What's the shared state I need from digit to digit?
data Env = EnvData
  { currentLine :: String,
    lineNumber :: Integer,
    column :: Column,
    parsingStartedAt :: Maybe Column,
    currentlyParsingValue :: String,
    parsedNumbers :: [ParsedNumber],
    parsedSymbols :: [Symbol]
  }
  deriving (Show)

isEngineSymbol :: Char -> Bool
isEngineSymbol c = c `elem` ['*', '#', '+', '$']

type Parser = Env -> String -> Env

-- TODO: This is doing too much. Should use a monad pattern for managing all the state stuff
parseChar :: Parser
parseChar env@(EnvData {
    currentlyParsingValue = n,
    parsingStartedAt = psa,
    lineNumber = ln,
    column = col
  }) input =
  case input of
    [] -> env
    (c : cs)
      | isDigit c ->
          parseChar
                ( env
                    { currentlyParsingValue = n ++ [c],
                      parsingStartedAt = if null psa then Just col else psa,
                      currentLine = cs,
                      column = col + 1
                    }
                )
                cs
      -- TODO: Store parsed symbols
      | isEngineSymbol c -> next "" cs
      -- Store the value we were parsing when it's done
      | not (null (currentlyParsingValue env)) ->
          let lineNumber' = LineNumber ln
              number = fromMaybe 0 $ readMaybe n
              psa' = fromMaybe col psa
              parsedNumber = PartNumber number (lineNumber', (psa', col - 1))
              -- Use col - 1 in about because c is the character after the value
           in parseChar
                ( env
                    { currentlyParsingValue = "",
                      parsingStartedAt = Nothing,
                      parsedNumbers = parsedNumber : parsedNumbers env,
                      currentLine = cs,
                      column = col + 1
                    }
                )
                cs
      | otherwise ->
          parseChar
            ( env
                { currentLine = cs,
                  column = col + 1
                }
            )
            cs
  where
    next pv cs =
      parseChar
        ( env
            { currentlyParsingValue = pv,
              currentLine = cs,
              column = col + 1
            }
        )
        cs

runParser :: Parser -> String -> Env
runParser p i = p (EnvData i 0 (Column 0) Nothing "" [] []) i

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
      parseLine = runParser parseChar
      parsedLine = parseLine $ head ls
      -- parsedLines = map parseLine ls
      answer = "0"
  return $ show parsedLine

-- return $ show (answer == exampleSolution, answer)

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  return $ show ls
