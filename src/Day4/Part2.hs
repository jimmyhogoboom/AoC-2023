module Day4.Part2 (cardNumber, cardWinMap, getCopies) where

import Data.Maybe (fromJust)
import Day4.Part1
import RIO.State (State, get, put, runState)
import qualified RIO.HashMap as HM

type WinMap = HM.HashMap CardNumber Integer

cardNumber :: ParsedCard -> CardNumber
cardNumber (ParsedCard c _ _) = c

findWinsRecord :: WinMap -> CardNumber -> Maybe Integer
findWinsRecord winsMap cn = HM.lookup cn winsMap

winnerCount :: ParsedCard -> Integer
winnerCount = toInteger . length . winners

cardWinMap :: [ParsedCard] -> WinMap
cardWinMap c = HM.fromList $ map go c
  where
    go card = (cardNumber card, winnerCount card)

data CopyState = CopyState
  { copies :: [CardNumber],
    newCopies :: [CardNumber]
  }
  deriving (Show)

copiesWon :: WinMap -> CardNumber -> [CardNumber]
copiesWon wm cardNum@(CardNumber cn) =
  let winsRec = findWinsRecord wm cardNum
      wins = fromJust winsRec
      start = cn + 1
      end = cn + wins
   in map CardNumber [start .. end]

getCopies :: WinMap -> [CardNumber] -> ([CardNumber], CopyState)
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
