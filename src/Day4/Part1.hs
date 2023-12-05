module Day4.Part1 where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)
import Import

wordsWhen :: (Char -> Bool) -> [Char] -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn s = wordsWhen (== s)

dualSplit :: Char -> String -> Maybe (String, String)
dualSplit s = splitValues . splitOn s
  where
    splitValues v = case v of
      [v1, v2] -> Just (v1, v2)
      _ -> Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

newtype CardNumber = CardNumber Integer
  deriving (Show, Eq)

newtype WinningNumber = WinningNumber Integer
  deriving (Show, Eq)

newtype MyNumber = MyNumber Integer
  deriving (Show, Eq)

instance Num CardNumber where
  CardNumber l1 + CardNumber l2 = CardNumber (l1 + l2)
  CardNumber l1 - CardNumber l2 = CardNumber (l1 - l2)
  CardNumber l1 * CardNumber l2 = CardNumber (l1 * l2)
  fromInteger = CardNumber
  negate (CardNumber ln) = CardNumber (negate ln)
  abs (CardNumber ln) = CardNumber (abs ln)
  signum (CardNumber ln) = CardNumber (signum ln)

instance Num WinningNumber where
  WinningNumber l1 + WinningNumber l2 = WinningNumber (l1 + l2)
  WinningNumber l1 - WinningNumber l2 = WinningNumber (l1 - l2)
  WinningNumber l1 * WinningNumber l2 = WinningNumber (l1 * l2)
  fromInteger = WinningNumber
  negate (WinningNumber ln) = WinningNumber (negate ln)
  abs (WinningNumber ln) = WinningNumber (abs ln)
  signum (WinningNumber ln) = WinningNumber (signum ln)

instance Num MyNumber where
  MyNumber l1 + MyNumber l2 = MyNumber (l1 + l2)
  MyNumber l1 - MyNumber l2 = MyNumber (l1 - l2)
  MyNumber l1 * MyNumber l2 = MyNumber (l1 * l2)
  fromInteger = MyNumber
  negate (MyNumber ln) = MyNumber (negate ln)
  abs (MyNumber ln) = MyNumber (abs ln)
  signum (MyNumber ln) = MyNumber (signum ln)

data ParsedCard = ParsedCard CardNumber [WinningNumber] [MyNumber]
  deriving (Show)

-- Split on ':' -> [cardNumber, cardValues]
-- cardNumber splitOn ' ' -> [_, cardNumber]
-- cardValues splitOn '|' -> [winningNumbers, cardNumbers]
-- [winningNumbers, cardNumbers] splitOn ' ' -> [[winningNumbers], [cardNumbers]]

cardNumber :: String -> Maybe CardNumber
cardNumber input = do
  (numString, _) <- dualSplit ':' input
  (_, cardNum) <- dualSplit ' ' numString
  return $ CardNumber $ read cardNum

cardPicks :: String -> Maybe ([WinningNumber], [MyNumber])
cardPicks input = do
  (_, nums) <- dualSplit ':' input
  (winners, mine) <- dualSplit '|' nums
  let winners' = map (WinningNumber . read) $ splitOn ' ' $ trim winners
      mine' = map (MyNumber . read) $ splitOn ' ' $ trim mine
  return (winners', mine')

parseCard :: String -> Maybe ParsedCard
parseCard input = do
  num <- cardNumber input
  (wins, mine) <- cardPicks input
  return $ ParsedCard num wins mine
  
parseLines :: [String] -> Maybe [ParsedCard]
parseLines il = go ([], il)
  where
    go (cards, inputLines) =
      case inputLines of
        (ln : ls) -> do
          card <- parseCard ln
          go (card : cards, ls)
        [] -> Just cards

-- parsePartNumbers :: [String] -> [ParsedNumber]
-- parsePartNumbers ls =
--   let parsedLines = parseLines ls
--       symbols = parsedSymbols parsedLines
--       nums = parsedNumbers parsedLines
--       identifiedNumbers = map (idPart symbols) nums
--    in onlyParts identifiedNumbers
