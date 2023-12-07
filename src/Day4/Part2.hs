module Day4.Part2 where

import Control.Monad (join)
import Data.Foldable (find)
import Data.Maybe (fromMaybe, fromJust)
import Day4.Part1

winnerCount :: ParsedCard -> Integer
winnerCount = toInteger . length . winners

-- for each card
-- get list of IDs of copies it generates
-- i.e winnerCount Card 1 -> 4 matches, so give [2, 3, 4, 5] (current ID + 1 .. numWins + 1)
--     winnerCount Card 2 -> 2 matches, so give [3, 4]
--     winnerCount Card 3 -> 2 matches, so give [4, 5]
--     winnerCount Card 4 -> 1 matches, so give [5]
--     winnerCount Card 5 -> 0 matches, so give []
--     winnerCount Card 6 -> 0 matches, so give []

-- copies generated: [2, 3, 3, 4, 4, 4, 5, 5, 5]

-- now go through each copy, and generate its copies
-- now go through those, etc.
-- until no copies are generated

cardNumber :: ParsedCard -> CardNumber
cardNumber (ParsedCard c _ _) = c

newtype WinMapEntry = WinMapEntry (CardNumber, Integer)
  deriving (Show)

findWinsRecord :: [WinMapEntry] -> CardNumber -> Maybe WinMapEntry
findWinsRecord winsMap (CardNumber cn) = find (\(WinMapEntry (CardNumber c, _)) -> c == cn) winsMap

getCopies :: [WinMapEntry] -> ([CardNumber], [CardNumber]) -> [CardNumber]
getCopies winsMap (prevCopies, cardNum@(CardNumber cn) : rest) =
  -- TODO: you have to stop when 'rest' is empty, too
  case winsRec of
    Just _ -> getCopies winsMap (prevCopies ++ copies, rest)
    Nothing -> prevCopies
  where
    -- winsRec = find (\(WinMapEntry (CardNumber c, _)) -> c == cn) winMap -- find CardNumber in map
    winsRec = findWinsRecord winsMap cardNum
    WinMapEntry (_, wins) = fromJust winsRec
    start = cn + 1
    end = cn + wins
    copies = map CardNumber [start .. end]
getCopies _ _ = []

cardWinMap :: [ParsedCard] -> [WinMapEntry]
cardWinMap = map $ WinMapEntry . go
  where
    go card = (cardNumber card, winnerCount card)

-- getAllCopies :: [ParsedCard] -> [CardNumber]
-- getAllCopies cards = cardNumbers >>= getCopies winMap
--   where
--     winMap = cardWinMap cards
--     cardNumbers = map cardNumber cards
-- getAllCopies cards = map (getCopies winMap) cardNumbers
--   where
--     winMap = cardWinMap cards
--     cardNumbers = map cardNumber cards

-- getCopiesOfCopies :: WinMap -> [CardNumber] -> [CardNumber]
-- getCopiesOfCopies winMap copies = join $ map wins copies
--   where wins c = -- find CardNumber in map

-- get count of each card ID by flattening (including the original cards)
-- so [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6]
--
-- and win counts = [1 => 4, 2 => 2, 3 => 2, 4 => 1]
-- then multiply the count of each number by the count of the copies it makes (wins)
-- for 1 = 1 * 4 = 4
--     2 = 2 * 2 = 4
--     3 = 3 * 2 = 6
--     4 = 4 * 1 = 6
--     5 = 4 * 0
--     6 = 1 * 0
--               = 20
--
--
