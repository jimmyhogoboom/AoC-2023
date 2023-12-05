module Day3.Types where

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
