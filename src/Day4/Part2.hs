module Day4.Part2 (cardNumber, cardWinMap, getCopies, countCopies, onlyCounts) where

import Data.Maybe (fromJust)
import Day4.Part1
import RIO.State (State, get, put, runState)
import qualified RIO.HashMap as HM

type WinMap = HM.HashMap CardNumber Integer

type WinCount = Integer
type CardCount = Integer

type CardMap = HM.HashMap CardNumber (WinCount, CardCount)

cardNumber :: ParsedCard -> CardNumber
cardNumber (ParsedCard c _ _) = c

findWinsRecord :: CardMap -> CardNumber -> Maybe (WinCount, CardCount)
findWinsRecord cardMap cn = HM.lookup cn cardMap

winnerCount :: ParsedCard -> Integer
winnerCount = toInteger . length . winners

cardWinMap :: [ParsedCard] -> CardMap
cardWinMap c = HM.fromList $ map go c
  where
    go card = (cardNumber card, (winnerCount card, 1))

data CopyState = CopyState
  { copies :: [CardNumber],
    newCopies :: [CardNumber]
  }
  deriving (Show)

-- for each CardNumber in the cardsToIncrement range
-- find its record
-- increment its CardCount by 1
copiesWon :: CardMap -> CardNumber -> CardMap
copiesWon cm cardNum@(CardNumber cn) =
  let winsRec = findWinsRecord cm cardNum
      (wins, prevCount) = fromJust winsRec
      start = cn + 1
      end = cn + wins
      cardsToIncrement = map CardNumber [start .. end]
   in case winsRec of
      Just _ -> go prevCount cardsToIncrement cm
      Nothing -> cm
   where go inc cards newMap =
          case cards of
            (c : cs) ->
              let (wins, count) = fromJust $ findWinsRecord newMap c
                  in go count cs (HM.insert c (wins, count + inc) newMap)
            [] -> newMap

getCopies :: CardMap -> (CardMap, CardMap)
getCopies =
  runState (go (CardNumber 1))
  where
    go :: CardNumber -> State CardMap CardMap
    go cn = do
      cardMap <- get
      let isMember = HM.member cn cardMap
          newMap = copiesWon cardMap cn
      if isMember
      then do 
        put newMap
        go (cn + 1)
      else return cardMap

onlyCounts :: CardMap -> [Integer]
onlyCounts = HM.foldl' go []
   where go :: [Integer] -> (WinCount, CardCount) -> [Integer]
         go acc (_, copyCount) = copyCount : acc

countCopies :: CardMap -> Integer
countCopies = HM.foldl' go 0
  where go acc (_, copyCount) = acc + copyCount
