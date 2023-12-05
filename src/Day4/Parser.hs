-- ChatGPT helped me refactor the parser into somethig using the State monad
-- This is for my own learning, and not used in any of my solutions

-- import Day4.Types

-- import Control.Monad.State
import Import
import Data.Char (isDigit)

data EnvData = EnvData
  { currentLine :: String,
    currentlyParsingValue :: String,
    parsingStartedAt :: Maybe Int,
    lineNumber :: Int,
    column :: Int,
    parsedNumbers :: [ParsedNumber],
    parsedSymbols :: [ParsedSymbol]
  }

type ParsedNumber = OtherNumber -- Assuming OtherNumber is a data constructor for numbers
type ParsedSymbol = (OtherSymbol, (Int, (Int, Int))) -- Assuming OtherSymbol is a data constructor for symbols

-- Define the Parser monad
type Parser = State EnvData

-- Helper function to parse a single character
parseChar :: Char -> Parser ()
parseChar c = do
  env <- get
  case c of
    -- Handle digit
    _ | isDigit c -> handleDigit c
    -- Handle symbol
    _ | isGearSymbol c -> handleSymbol c
    -- Handle other cases
    _ -> handleOther c

-- Helper function to handle digit
handleDigit :: Char -> Parser ()
handleDigit c = do
  env <- get
  let newEnv =
        env
          { currentlyParsingValue = currentlyParsingValue env ++ [c],
            parsingStartedAt = if null (parsingStartedAt env) then Just (column env) else parsingStartedAt env,
            column = column env + 1
          }
  put newEnv
  parseLine

-- Helper function to handle symbol
handleSymbol :: Char -> Parser ()
handleSymbol c = do
  env <- get
  let symbol = (OtherSymbol [c], (lineNumber env, (column env, column env)))
  let newEnv =
        env
          { currentlyParsingValue = "",
            parsingStartedAt = Nothing,
            parsedSymbols = symbol : parsedSymbols env,
            column = column env + 1
          }
  put newEnv
  parseLine

-- Helper function to handle other cases
handleOther :: Char -> Parser ()
handleOther c = do
  env <- get
  let doStore = not (null (currentlyParsingValue env))
  case c of
    _ | doStore -> handleStore
    _ -> handleDefault

-- Helper function to handle store
handleStore :: Parser ()
handleStore = do
  env <- get
  let number = fromMaybe 0 $ readMaybe (currentlyParsingValue env)
  let psa' = fromMaybe (column env) (parsingStartedAt env)
  let parsedNumber = OtherNumber number (lineNumber env, (psa', column env - 1))
  let newEnv =
        env
          { currentlyParsingValue = "",
            parsingStartedAt = Nothing,
            parsedNumbers = parsedNumber : parsedNumbers env,
            column = column env + 1
          }
  put newEnv
  parseLine

-- Helper function to handle default case
handleDefault :: Parser ()
handleDefault = do
  env <- get
  let newEnv = env {column = column env + 1}
  put newEnv
  parseLine

-- Main parseLine function using the Parser monad
parseLine :: Parser ()
parseLine = do
  env <- get
  case currentLine env of
    [] -> do
      let newEnv = env {currentLine = ""}
      put newEnv
    (c : cs) -> parseChar c

-- Assuming you have a readMaybe function imported for the readMaybe calls

