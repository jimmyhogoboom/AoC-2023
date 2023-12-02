{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day1
import Day2

-- TODO: make tool to convert list of parts into these pairs
type DayResult = [(String, String)]

day1 :: RIO App DayResult
day1 = do
  p1 <- liftIO Day1.part1
  p2 <- liftIO Day1.part2
  return [("1.1", p1), ("1.2", p2)]

day2 :: RIO App DayResult
day2 = do
  p1 <- liftIO Day2.part1
  -- TODO: p2
  return [("2.1", p1)]

run :: RIO App ()
run = do
  d1 <- day1
  d2 <- day2
  -- TODO: Accept command line arguments to choose the day to print
  logInfo $ displayShow [d1, d2]
