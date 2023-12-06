module Day4.Part1 where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

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

fromWinningNumber :: WinningNumber -> Integer
fromWinningNumber (WinningNumber n) = n

fromMyNumber :: MyNumber -> Integer
fromMyNumber (MyNumber n) = n

cardWinners :: ParsedCard -> [Integer]
cardWinners (ParsedCard _ winNums _) = map fromWinningNumber winNums

cardMyNumbers :: ParsedCard -> [Integer]
cardMyNumbers (ParsedCard _ _ mine) = map fromMyNumber mine

parseCardNumber :: String -> Maybe CardNumber
parseCardNumber input = do
  (numString, _) <- dualSplit ':' input
  (_, cardNum) <- dualSplit ' ' numString
  return $ CardNumber $ read cardNum

parseCardPicks :: String -> Maybe ([WinningNumber], [MyNumber])
parseCardPicks input = do
  (_, nums) <- dualSplit ':' input
  (wins, mine) <- dualSplit '|' nums
  let wins' = map (WinningNumber . read) $ splitOn ' ' $ trim wins
      mine' = map (MyNumber . read) $ splitOn ' ' $ trim mine
  return (wins', mine')

parseCard :: String -> Maybe ParsedCard
parseCard input = do
  num <- parseCardNumber input
  (wins, mine) <- parseCardPicks input
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

winners :: ParsedCard -> [Integer]
winners c = intersection (cardWinners c) (cardMyNumbers c)
  where 
    intersection :: [Integer] -> [Integer] -> [Integer]
    intersection a b = filter (`elem` b) a

score :: [Integer] -> Integer
score vals = case vals of
  [] -> 0
  [_] -> 1
  _ -> foldl (\s _ -> s * 2) 1 [2..len]
  where len = toInteger $ length vals
