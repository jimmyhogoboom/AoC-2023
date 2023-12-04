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

-- TODO: refactor using a Reader or some other high-level pattern:
-- ex :: Reader SharedState Result
-- ex = do
--   sharedState <- ask
--   return $ funThatDoesSomethingWithState sharedState
--

newtype LineNumber = LineNumber Integer
  deriving (Show, Eq)

instance Num LineNumber where
  LineNumber l1 + LineNumber l2 = LineNumber (l1 + l2)
  LineNumber l1 - LineNumber l2 = LineNumber (l1 - l2)
  LineNumber l1 * LineNumber l2 = LineNumber (l1 * l2)
  fromInteger = LineNumber
  negate (LineNumber ln) = LineNumber (negate ln)
  abs (LineNumber ln) = LineNumber (abs ln)
  signum (LineNumber ln) = LineNumber (signum ln)

newtype Column = Column Integer
  deriving (Show, Eq, Ord)

instance Num Column where
  Column c1 + Column c2 = Column (c1 + c2)
  Column c1 - Column c2 = Column (c1 - c2)
  Column l1 * Column l2 = Column (l1 * l2)
  fromInteger = Column
  negate (Column ln) = Column (negate ln)
  abs (Column ln) = Column (abs ln)
  signum (Column ln) = Column (signum ln)

type Location = (LineNumber, (Column, Column))

data ParsedNumber = PartNumber Integer Location | OtherNumber Integer Location
  deriving (Show)

data Symbol = Symbol String Location
  deriving (Show)

-- What's the shared state I need from digit to digit?
data Env = EnvData
  { currentLine :: String,
    lineNumber :: LineNumber,
    column :: Column,
    parsingStartedAt :: Maybe Column,
    currentlyParsingValue :: String,
    parsedNumbers :: [ParsedNumber],
    parsedSymbols :: [Symbol]
  }
  deriving (Show)

type Parser = Env -> Env

isEngineSymbol :: Char -> Bool
isEngineSymbol c = notElem c "." && not (isDigit c)

-- TODO: This is doing too much. Should use a monad pattern for managing all the state stuff
parseLine :: Parser
parseLine
  env@( EnvData
          { currentLine = cl,
            currentlyParsingValue = n,
            parsingStartedAt = psa,
            lineNumber = ln,
            column = col
          }
        ) =
    case cl of
      [] ->
        let doStore = not (null (currentlyParsingValue env))
         in if doStore
              then
                let -- only parsing multi-digit numbers for now
                    number = fromMaybe 0 $ readMaybe n
                    psa' = fromMaybe col psa
                    parsedNumber = OtherNumber number (ln, (psa', col - 1))
                 in -- Use col - 1 in about because c is the character after the value
                    ( env
                        { currentlyParsingValue = "",
                          parsingStartedAt = Nothing,
                          parsedNumbers = parsedNumber : parsedNumbers env,
                          currentLine = "",
                          column = col + 1
                        }
                    )
              else
                ( env
                    { currentlyParsingValue = "",
                      parsingStartedAt = Nothing,
                      currentLine = "",
                      column = col + 1
                    }
                )
      (c : cs)
        | isDigit c ->
            parseLine
              ( env
                  { currentlyParsingValue = n ++ [c],
                    parsingStartedAt = if null psa then Just col else psa,
                    currentLine = cs,
                    column = col + 1
                  }
              )
        -- Store a symbol immediately
        | isEngineSymbol c ->
            let symbol = Symbol [c] (ln, (col, col))
                doStore = not (null (currentlyParsingValue env))
             in if doStore
                  then
                    let -- only parsing multi-digit numbers for now
                        number = fromMaybe 0 $ readMaybe n
                        psa' = fromMaybe col psa
                        parsedNumber = OtherNumber number (ln, (psa', col - 1))
                     in -- Use col - 1 in about because c is the character after the value
                        parseLine
                          ( env
                              { currentlyParsingValue = "",
                                parsingStartedAt = Nothing,
                                parsedNumbers = parsedNumber : parsedNumbers env,
                                parsedSymbols = symbol : parsedSymbols env,
                                currentLine = cs,
                                column = col + 1
                              }
                          )
                  else
                    parseLine
                      ( env
                          { currentlyParsingValue = "",
                            parsingStartedAt = Nothing,
                            parsedSymbols = symbol : parsedSymbols env,
                            currentLine = cs,
                            column = col + 1
                          }
                      )
        -- Store the value we were parsing when it's done
        | not (null (currentlyParsingValue env)) ->
            let -- only parsing multi-digit numbers for now
                number = fromMaybe 0 $ readMaybe n
                psa' = fromMaybe col psa
                parsedNumber = OtherNumber number (ln, (psa', col - 1))
             in -- Use col - 1 in about because c is the character after the value
                parseLine
                  ( env
                      { currentlyParsingValue = "",
                        parsingStartedAt = Nothing,
                        parsedNumbers = parsedNumber : parsedNumbers env,
                        currentLine = cs,
                        column = col + 1
                      }
                  )
        | otherwise ->
            parseLine
              ( env
                  { currentLine = cs,
                    column = col + 1
                  }
              )

lineNumberOfPart :: ParsedNumber -> LineNumber
lineNumberOfPart (PartNumber _ (ln, _)) = ln
lineNumberOfPart (OtherNumber _ (ln, _)) = ln

boundsOfPart :: ParsedNumber -> (Column, Column)
boundsOfPart (PartNumber _ (_, bounds)) = bounds
boundsOfPart (OtherNumber _ (_, bounds)) = bounds

partNumber :: ParsedNumber -> Integer
partNumber (PartNumber n (_, _)) = n
partNumber (OtherNumber n (_, _)) = n

lineNumberOfSymbol :: Symbol -> LineNumber
lineNumberOfSymbol (Symbol _ (ln, _)) = ln

indexOfSymbol :: Symbol -> Column
indexOfSymbol (Symbol _ (_, (i, _))) = i

-- get list of symbols on line above
symbolsAboveNumber :: [Symbol] -> ParsedNumber -> [Symbol]
symbolsAboveNumber symbols parsedNumber =
  filter (\s -> lineNumberOfPart parsedNumber == lineNumberOfSymbol s + 1) symbols

-- get list of symbols on line below
symbolsBelowNumber :: [Symbol] -> ParsedNumber -> [Symbol]
symbolsBelowNumber symbols parsedNumber =
  filter (\s -> lineNumberOfPart parsedNumber == lineNumberOfSymbol s - 1) symbols

symbolsOnSameLine :: [Symbol] -> ParsedNumber -> [Symbol]
symbolsOnSameLine symbols parsedNumber =
  filter (\s -> lineNumberOfPart parsedNumber == lineNumberOfSymbol s) symbols

indexInRange :: (Column, Column) -> Column -> Bool
indexInRange (startI, endI) index = index >= startI && index <= endI

-- get list of symbols between column range
symbolsInRange :: [Symbol] -> ParsedNumber -> [Symbol]
symbolsInRange symbols parsedNumber =
  filter match symbols
  where
    (startI, endI) = boundsOfPart parsedNumber
    validRange = (startI - 1, endI + 1)
    match s = indexInRange validRange (indexOfSymbol s)

partNumberFromOther :: ParsedNumber -> ParsedNumber
partNumberFromOther i = case i of
  OtherNumber n l -> PartNumber n l
  _ -> undefined

-- check line above, from index start -1 to end + 1
-- check the same line, at indexes start - 1 and end + 1
-- check line below, from index start -1 to end + 1
-- If symbols in any of above, number is a PartNumber
idPart :: [Symbol] -> ParsedNumber -> ParsedNumber
idPart symbols otherNumber
  | hasSymbol = partNumberFromOther otherNumber
  | otherwise = otherNumber
  where
    nearLine =
      join
        [ symbolsBelowNumber symbols otherNumber,
          symbolsAboveNumber symbols otherNumber,
          symbolsOnSameLine symbols otherNumber
        ]
    inRange = symbolsInRange nearLine otherNumber
    hasSymbol = not $ null inRange

onlyParts :: [ParsedNumber] -> [ParsedNumber]
onlyParts = filter isPart
  where
    isPart p = case p of
      (PartNumber _ _) -> True
      _ -> False

sumOfParts :: [ParsedNumber] -> Integer
sumOfParts = sum . numbers
  where
    numbers = map partNumber

envFromString :: String -> Env
envFromString i = EnvData i 0 (Column 0) Nothing "" [] []

parseLines :: [String] -> Env
parseLines il = go (envFromString "", il)
  where
    go (inputEnv, inputLines) =
      case inputLines of
        (ln : ls) -> go (parseLine (nextEnv inputEnv ln), ls)
        [] -> inputEnv
    nextEnv env@(EnvData {lineNumber = ln}) l =
      env {currentLine = l, lineNumber = ln + 1, column = 0}

parsePartNumbers :: [String] -> [ParsedNumber]
parsePartNumbers ls =
  let parsedLines = parseLines ls
      symbols = parsedSymbols parsedLines
      nums = parsedNumbers parsedLines
      identifiedNumbers = map (idPart symbols) nums
   in onlyParts identifiedNumbers

part1 :: IO String
part1 = do
  contents <- readFile file
  let ls = lines contents
      parts = parsePartNumbers ls
      answer = sumOfParts parts
      partNumbers = map partNumber parts
  return $ show answer

part2 :: IO String
part2 = do
  contents <- readFile file
  let ls = lines contents
  return $ show ls
