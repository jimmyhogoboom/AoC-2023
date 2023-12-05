module Day3.Part1 where

import Day3.Types

import Data.Char (isDigit)
import Import

-- TODO: refactor using a Reader or some other high-level pattern:
-- ex :: Reader SharedState Result
-- ex = do
--   sharedState <- ask
--   return $ funThatDoesSomethingWithState sharedState
--

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
