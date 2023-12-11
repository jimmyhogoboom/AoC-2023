module Day4.Part2 (cardNumber, cardWinMap, getCopies) where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Day4.Part1
import RIO.State (State, get, put, runState)

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

cardWinMap :: [ParsedCard] -> [WinMapEntry]
cardWinMap = map $ WinMapEntry . go
  where
    go card = (cardNumber card, winnerCount card)

data CopyState = CopyState
  { copies :: [CardNumber],
    newCopies :: [CardNumber]
  }
  deriving (Show)

copiesWon :: [WinMapEntry] -> CardNumber -> [CardNumber]
copiesWon wm cardNum@(CardNumber cn) =
  let winsRec = findWinsRecord wm cardNum
      WinMapEntry (_, wins) = fromJust winsRec
      start = cn + 1
      end = cn + wins
   in map CardNumber [start .. end]

getCopies :: [WinMapEntry] -> [CardNumber] -> ([CardNumber], CopyState)
getCopies _ [] = ([], CopyState {newCopies = [], copies = []})
getCopies initialWinMap initialCopies =
  runState go CopyState {newCopies = initialCopies, copies = []}
  where
    go :: State CopyState [CardNumber]
    go = do
      CopyState
        { copies = c,
          newCopies = nc
        } <-
        get
      case nc of
        (cn : ncs) -> do
          let copiesWon' = copiesWon initialWinMap cn
          put $
            CopyState
              { copies = cn : c,
                newCopies = ncs ++ copiesWon'
              }
          go
        [] -> return c
