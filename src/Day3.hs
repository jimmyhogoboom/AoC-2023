module Day3
  ( part1,
    part2,
  )
where

import Control.Applicative (Alternative (empty))
import Data.Char (isDigit, isSpace, isSymbol)
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

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

data EngineValue
  = EngineNumber Integer
  | EngineSymbol String
  | EngineNull
  deriving (Show, Eq)

numberP :: Parser EngineValue
numberP = EngineNumber . read <$> notNull (spanP isDigit)

symbolP :: Parser EngineValue
symbolP = EngineSymbol <$> notNull (spanP isEngineSymbol)

separatorP :: Parser String
separatorP = undefined

isEngineSymbol :: Char -> Bool
isEngineSymbol c = c `elem` ['*', '#', '+', '$']

engineValue :: Parser EngineValue
engineValue = numberP <|> symbolP

-- | Creates a parser for a string of type "element1 sep1 element2 sep2 element3"
-- from a parser for separators (sep1, sep2) and and a parser form elements (element1, element2, element3).
sepBy :: Parser a   -- Parser for the separators
      -> Parser b   -- Parser for elements
      -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

parseFile fileName parser = do
  input <- readFile fileName
  case runParser parser input of
    Just (x, y)       -> return (x, y)
    Nothing -> return ("", EngineNull)

part1 :: IO String
part1 = do
  -- contents <- readFile file
  result <- parseFile file engineValue
  -- let ls = lines contents
  --     line = head ls
  --     parsedLine = runParser engineValue line
      -- answer = "0"
  -- return $ show (answer == exampleSolution, answer)
  return $ show result

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  return $ show ls
