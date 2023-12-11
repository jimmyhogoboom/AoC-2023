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
