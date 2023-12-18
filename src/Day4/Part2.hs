module Day4.Part2 (cardNumber, cardWinMap, getCopies, countCopies, onlyCounts, sortedMap, formatMap) where

import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Day4.Part1
import qualified RIO.HashMap as HM
import RIO.State (State, get, put, runState)

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

-- for each CardNumber in the cardsToIncrement range
-- find its record
-- increment its CardCount by 1
copiesWon :: CardMap -> CardNumber -> CardMap
copiesWon cm cardNum@(CardNumber cn) =
  let winsRec = findWinsRecord cm cardNum
      (wins, prevCount) = fromMaybe (0, 1) winsRec
      start = cn + 1
      end = cn + wins
      cardsToIncrement = map CardNumber [start .. end]
   in case winsRec of
        Just _ -> go prevCount cardsToIncrement cm
        Nothing -> cm
  where
    go inc cards newMap =
      case cards of
        (c : cs) ->
          let (wins, count) = fromMaybe (0, 1) $ findWinsRecord newMap c
           in go inc cs (HM.insert c (wins, count + inc) newMap)
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
  where
    go :: [Integer] -> (WinCount, CardCount) -> [Integer]
    go acc (_, copyCount) = copyCount : acc

countCopies :: CardMap -> Integer
countCopies = HM.foldl' go 0
  where
    go acc (_, copyCount) = acc + copyCount

sortCardMapEntry :: (CardNumber, (WinCount, CardCount)) -> (CardNumber, (WinCount, CardCount)) -> Ordering
sortCardMapEntry (CardNumber cn1, (_, cc1)) (CardNumber cn2, (_, cc2))
  | cn1 < cn2 = LT
  | cn1 > cn2 = GT
  | cn1 == cn2 = compareCounts cc1 cc2
  where
    compareCounts c1 c2
      | c1 < c2 = LT
      | c1 > c2 = GT
      | otherwise = EQ
sortCardMapEntry _ _ = EQ

sortedMap :: CardMap -> [(CardNumber, (WinCount, CardCount))]
sortedMap cardMap = sortBy sortCardMapEntry $ HM.toList cardMap

formatMap :: CardMap -> String
formatMap input = go "" (sortedMap input)
  where
    go result entries =
      case entries of
        (e : es) -> go (result ++ show e ++ ['\n']) es
        _ -> result
