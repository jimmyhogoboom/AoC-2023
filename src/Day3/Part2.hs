module Day3.Part2 where

import Data.Char (isDigit)
import Day3.Types hiding
  ( Env,
    EnvData,
    Parser,
    Symbol,
    column,
    currentLine,
    currentlyParsingValue,
    indexOfSymbol,
    lineNumber,
    lineNumberOfSymbol,
    parsedNumbers,
    parsedSymbols,
    parsingStartedAt,
  )
import Import

data Symbol = Gear String [ParsedNumber] | OtherSymbol String
  deriving (Show)

type ParsedSymbol = (Symbol, Location)

data Env = EnvData
  { currentLine :: String,
    lineNumber :: LineNumber,
    column :: Column,
    parsingStartedAt :: Maybe Column,
    currentlyParsingValue :: String,
    parsedNumbers :: [ParsedNumber],
    parsedSymbols :: [ParsedSymbol]
  }
  deriving (Show)

type Parser = Env -> Env

lineNumberOfSymbol :: ParsedSymbol -> LineNumber
lineNumberOfSymbol (_, (ln, _)) = ln

indexOfSymbol :: ParsedSymbol -> Column
indexOfSymbol (_, (_, (i, _))) = i

numbersOfGear :: ParsedSymbol -> [Integer]
numbersOfGear (Gear _ [OtherNumber p1 _, OtherNumber p2 _], _) = [p1, p2]
numbersOfGear _ = []

isGearSymbol :: Char -> Bool
isGearSymbol = (== '*')

-- TODO: This is doing too much. Refactor using a Reader or some other high-level pattern:
-- ex :: Reader SharedState Result
-- ex = do
--   sharedState <- ask
--   return $ funThatDoesSomethingWithState sharedState
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
        | isGearSymbol c ->
            let symbol = (OtherSymbol [c], (ln, (col, col)))
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

-- get list of numbers on line above
numbersAboveSymbol :: [ParsedNumber] -> ParsedSymbol -> [ParsedNumber]
numbersAboveSymbol numbers parsedSymbol =
  filter (\n -> lineNumberOfSymbol parsedSymbol == lineNumberOfPart n + 1) numbers

-- get list of numbers on line below
numbersBelowSymbol :: [ParsedNumber] -> ParsedSymbol -> [ParsedNumber]
numbersBelowSymbol numbers parsedSymbol =
  filter (\n -> lineNumberOfSymbol parsedSymbol == lineNumberOfPart n - 1) numbers

numbersOnSameLine :: [ParsedNumber] -> ParsedSymbol -> [ParsedNumber]
numbersOnSameLine numbers parsedSymbol =
  filter (\n -> lineNumberOfSymbol parsedSymbol == lineNumberOfPart n) numbers

indexInRange :: (Column, Column) -> Column -> Bool
indexInRange (startI, endI) index = index >= startI && index <= endI

-- get list of numbers between column range
numbersInRange :: [ParsedNumber] -> ParsedSymbol -> [ParsedNumber]
numbersInRange numbers parsedSymbol =
  filter match numbers
  where
    validRange n =
      let (startI, endI) = boundsOfPart n
       in (startI - 1, endI + 1)
    match n = indexInRange (validRange n) (indexOfSymbol parsedSymbol)

partNumberFromOther :: ParsedNumber -> ParsedNumber
partNumberFromOther i = case i of
  OtherNumber n l -> PartNumber n l
  _ -> i

gearFromSymbol :: ParsedSymbol -> [ParsedNumber] -> ParsedSymbol
gearFromSymbol i ns = case i of
  (OtherSymbol s, l) -> (Gear s ns, l)
  _ -> i

onlyGears :: [ParsedSymbol] -> [ParsedSymbol]
onlyGears = filter isGear
  where
    isGear (s, _) = case s of
      (Gear _ _) -> True
      _ -> False

sumOfGears :: [ParsedSymbol] -> Integer
sumOfGears = sum . parts
  where
    parts = map (product . numbersOfGear)

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

idSymbol :: [ParsedNumber] -> ParsedSymbol -> ParsedSymbol
idSymbol parts symbol
  | isGear = gearFromSymbol symbol inRange
  | otherwise = symbol
  where
    nearLine =
      join
        [ numbersBelowSymbol parts symbol,
          numbersAboveSymbol parts symbol,
          numbersOnSameLine parts symbol
        ]
    gearNums = numbersInRange nearLine symbol
    inRange = case gearNums of
      [n1, n2] -> [n1, n2]
      _ -> []
    isGear = length inRange == 2

parseGears :: [String] -> [ParsedSymbol]
parseGears ls =
  let parsedLines = parseLines ls
      symbols = parsedSymbols parsedLines
      nums = parsedNumbers parsedLines
      identifiedSymbols = map (idSymbol nums) symbols
   in onlyGears identifiedSymbols
